.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2021-2026 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

=========
YDB AIM
=========

.. contents::
   :depth: 3

-----------
Overview
-----------

Applications need metadata. By providing an API to manage metadata, and enable content-based access to data, the YottaDB AIM (Application Independent Metadata) plugin removes the need for each YottaDB application to create its own separate framework.

.. _quickstart:

-------------
Quickstart
-------------

As a YottaDB plugin, AIM requires YottaDB. You can install YottaDB and AIM together:

.. code:: bash

   mkdir /tmp/tmp ; wget -P /tmp/tmp https://gitlab.com/YottaDB/DB/YDB/raw/master/sr_unix/ydbinstall.sh
   cd /tmp/tmp ; chmod +x ydbinstall.sh
   sudo ./ydbinstall.sh --utf8 --aim

Omit the ``--utf8`` option if you do not want UTF-8 support installed. If you already have YottaDB installed, use ``sudo $ydb_dist/ydbinstall --aim --plugins-only --overwrite-existing`` to install or reinstall the AIM plugin without reinstalling YottaDB.

When used to install `Octo <../Octo/>`_, ``ydbinstall`` / ``ydbinstall.sh`` automatically installs AIM, as it is a requirement for Octo.

-------------
Installation
-------------

If you don't use the :ref:`quickstart` method, you can install YDBAIM from source. In addition to YottaDB and its requirements, YDBAIM requires ``cmake``, ``gcc``, ``git``, ``make``, and ``pkg-config``. Clone the YDBAIM repository, and then install the plugin, using the following commands:

.. code:: bash

   git clone https://gitlab.com/YottaDB/Util/YDBAIM.git YDBAIM-master
   cd YDBAIM-master
   mkdir build && cd build
   cmake ..
   make && sudo make install

----------
Metadata
----------

++++++++++++++++++
Cross References
++++++++++++++++++

As an example consider a global variable whose first subscript is the year a US President assumed office and the second subscript is the year that President left office, with values such as:

.. code:: none

   ^USPresidents(1797,1801)="John||Adams" and
   ^USPresidents(1835,1839)="John|Quincy|Adams"

~~~~~~~~~~~~~~~~~~~~~~
Data Cross References
~~~~~~~~~~~~~~~~~~~~~~

The call :code:`$$XREFDATA^%YDBAIM("^USPresidents",2,"|",3)` cross references the third piece of node values (last names), with the cross reference global having values such as :code:`^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(3,"Adams",1797,1801)=""`, :code:`^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(3,"Adams",1835,1839)=""`, and many others including :code:`^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(3,"Obama",2009,2017)=""`.

Suppose only Presidents who left office in the 19th century should be cross referenced. A local variable node such as :code:`cent19(2)="1801:1899"` can be created, and passed by reference, and :code:`$$XREFDATA^%YDBAIM("^USPresidents",.cent19,"|",3)` produces the two cross references as above, but in a different global variable name since the trigger signatures are different. Unlike the first cross reference, this does not cross reference Barack Obama who assumed office in 2009 and left in 2017.

Suppose only Presidents who assumed office in the 19th century should be cross referenced, a local variable :code:`cent19` would instead have the node :code:`cent19(1)="1801:1899"` to indicate that only first subscripts should be cross referenced if they are in the 19th century, but the local variable root node :code:`cent19=2` should be set to indicate that two subscripts should be cross referenced. In this case, the call :code:`$$XREFDATA^%YDBAIMD("^USPresidents",.cent19,"|",3)` generates a cross reference that includes John Quincy Adams, but not John Adams who assumed office in 1797, which is in the 18th century.

To cross reference all three names, the call :code:`$$XREFDATA^%YDBAIM("^USPresidents",2,"|","1:3")` is used to generate the following cross references for the two President Adams:

.. code:: none

   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(1,"John",1797,1801)=""
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(1,"John",1835,1839)=""
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(2,"",1797,1801)=""
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(2,"Quincy",1835,1839)=""
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(3,"Adams",1797,1801)=""
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(3,"Adams",1835,1839)=""

Since the first President Adams record does not include a middle name, the corresponding record has an empty string ("") subscript. *Any region to which ^%ydbAIM\* global variables are mapped must have NULL_SUBSCRIPTS set to ALWAYS.* Since the subscripts will include pieces of global nodes, or even entire global nodes, it would be prudent to set YottaDB's maximum key size (1019 bytes) for that region.

.. note::

   Subscript specifications which are not canonical numbers should be quoted. So to cross reference the first piece with "|" separator of :code:`^%ydbocto("tables","pg_catalog","pg_attribute",*)`:

     .. code:: bash

        YDB>set sub=4,sub(1)="""tables""",sub(2)="""pg_catalog""",sub(3)="""pg_attribute"""
        YDB>set xref=$$XREFDATA^%YDBAIM("^%ydbocto",.sub,"|",1)

   to create the cross reference:

     .. code:: bash

        YDB>write xref
        ^%ydbAIMDvjlGbD84bQ5u5hXGOtIe37
        YDB>

   Setting a value now creates the cross reference:

     .. code:: bash

        YDB>set ^%ydbocto("tables","pg_catalog","pg_attribute",100)="ABC|DEF"

        YDB>write $query(@xref@(1,"ABC",""))
        ^%ydbAIMDvjlGbD84bQ5u5hXGOtIe37(1,"ABC",100)
        YDB>


~~~~~~~~~~~~~~~~~~~~~~~~~~~
Subscript Cross References
~~~~~~~~~~~~~~~~~~~~~~~~~~~

AIM can also cross reference subscripts. To search values of subscripts other than the first subscript requires using `$ORDER() <../ProgrammersGuide/functions.html#order>`_ to loop through higher level subscripts, which can be time-consuming for a large dataset. The call ``$$XREFSUB^%YDBAIM("^USPresidents",2,2)`` cross references the second subscript of the two-subscript global variable ^USPresidents, with values such as:

.. code:: none

   ^%ydbAIMSrNrMckj7LkdFXjsHkuT91D(2,1913,1909)=""
   ^%ydbAIMSrNrMckj7LkdFXjsHkuT91D(2,1921,1913)=""
   ^%ydbAIMSrNrMckj7LkdFXjsHkuT91D(2,1923,1921)=""

The first subscript of ``^%ydbAIMSrNrMckj7LkdFXjsHkuT91D`` is the subscript number of the cross reference, in this case 2 for the second subscript. The second subscript of ``^%ydbAIMSrNrMckj7LkdFXjsHkuT91D`` are values of the cross referenced second subscript, and the third subscript is a corresponding first subscript for the cross referenced first subscript. So ``^%ydbAIMSrNrMckj7LkdFXjsHkuT91D(2,1921,1913)=""`` says that there is a node ``^USPresidents(1913.1921)``.

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Example - Using a Subscript Cross Reference
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This illustrates the use of :ref:`xrefsub` to replace scanning of application subscripts.

Consider a global variable ``^X(a,b,c)`` where an application needs to find all nodes whose second subscript (``b``) meets some criterion. In SQL terms, this is like a table with a primary key consisting of three columns, and statement SELECT a,b,c WHERE b meets some condition. This is illustrated in the M program `<scandemo2.m>`_; equivalent programs can be written in any supported language.

The program generates a global with a number of nodes. The number can be specified on the command line to run the program, e.g., ``yottadb -run scandemo2 10000``, with the number defaulting to 100,000 if not specified. Each of the subscripts is a random number from 0 through 999,999. The program then scans the global variable to find nodes (i.e., SELECTs from the table) that meet four different criteria. For each criterion, it scans in two ways, without using XREFSUB() and using XREFSUB(). In each case it prints the time taken and the number of nodes/rows found; the latter must be the same regardless of how the global variable is scanned.

- The first is to identify all nodes where b>750000, i.e., a simple numerical scan.
- The second is to scan all nodes where b follows "700000", i.e., a `lexical scan <#forcing>`_. For example, US zip codes are numeric, but should be ordered lexically (a numeric scan would ignore leading zeroes).
- The third is a scan based on the value of a 1:1 `transformation function <#transformation>`_, i.e., f(b) satisfies some criterion. A sample use case of such a transformation function is one where times are stored in local time, but need to be converted to UTC for processing / selection. In the `scandemo2 <scandemo2.m>`_ program, the 1:1 transformation is the square root of b ($$FUNC^%SQROOT(b)), and the selection criterion is nodes where the square root is greater than 750.
- The fourth is a scan based on the value of a many:1 transformation function. A hypothetical use case might be the checksum of other values, or where the values are the orbital parameters of a celestial object, and the transformation function is the next date that the object is likely to pass within 100,000 kilometers of Planet Earth. In the ``scandemo2`` program, the many:1 transformation function is the sum of the digits of the second subscript, and the selection criterion is nodes where the sum is greater than 30.

Sample output:

.. code:: bash

   $ yottadb -run scandemo2 10000
   Traversal without XREFSUB() took 4,209 microseconds for 2,495 nodes/rows
   Traversal with XREFSUB() took 2,933 microseconds for 2,495 nodes/rows
   Traversal without XREFSUB() using string collation took 4,961 microseconds for 2,795 nodes/rows
   Traversal with XREFSUB() using string collation took 3,494 microseconds for 2,795 nodes/rows
   Traversal without XREFSUB() using a 1:1 transformation function took 7,896 microseconds for 4,335 nodes/rows
   Traversal with XREFSUB() using a 1:1 transformation function took 5,769 microseconds for 4,335 nodes/rows
   Traversal without XREFSUB() using a many:1 transformation function took 8,049 microseconds for 3,139 nodes/rows
   Traversal with XREFSUB() using a many:1 transformation function took 3,041 microseconds for 3,139 nodes/rows
   $

.. _statistics:

+++++++++++++
Statistics
+++++++++++++

The optional parameter :code:`stat` can be used to instruct AIM that the application wishes to compute and maintain statistics. There are two types of statistics (the default, stat=0, is cross references only and no statistics):

* **stat=1**: statistics on the count of each value. Thus the call :code:`$$XREFDATA^%YDBAIM("^USPresidents",2,"|","1:3",,,,1)` would compute and maintain nodes such as :code:`^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-1,"John")=4` to indicate that "John" appears as the first piece four times (the first subscript is the negative of the piece number).

* **stat=2**: in addition to the count of each value, also counts the number of different values, and also a total count of the number of values maintained. Thus, the call :code:`$$XREFDATA^%YDBAIM("^USPresidents",2,"|","1:3",,,,2)` would compute and maintain nodes such as :code:`^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-3)=39` to indicate that there are 39 distinct last names and :code:`^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(11)=135` to indicate that there are 135 nodes maintained (as of 2021, the 45 former US Presidents times 3 names for each ex-President).

~~~~~~~~~~~~~~~~~~~~~
Example - Statistics
~~~~~~~~~~~~~~~~~~~~~

.. code:: bash

   YDB>set x=$$XREFDATA^%YDBAIM("^USPresidents",2,"|","1:3",,,,2)

   YDB>set z="" f i=1:1:5  s z=$o(@x@(-1,z)) zwr @x@(-1,z) ; count of each value
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-1,"Abraham")=1
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-1,"Andrew")=2
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-1,"Barack")=1
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-1,"Benjamin")=1
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-1,"Bill")=1

   YDB>zwrite %ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-3:-1),^(11) ; number of distinct values and number of total values
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-3)=39  ; 39 distinct last names
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-2)=14  ; 14 distinct middle names/initials
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-1)=31  ; 31 distinct first names
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(11)=135 ; a total of 135 nodes maintained

   YDB>

Statistics can be used to optimize queries. For example, if one wants a query where the first name is George (:code:`^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-1,"George")=3`), middle initial is W. (:code:`^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-2,"W.")=1`) and the last name is Bush (:code:`^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-3,"Bush")=2`), it would make sense to look at the middle initial first (since its count is one). This approach minimizes the number of global accesses.

Consider another example, a global variable that contains all of the U.S. State Capitals and you would like to find out which capitals are named after the Presidents. In this case it would make sense to look at the last names of the Presidents first, since there are 39 distinct last names and 50 distinct state capitals.

.. _schematype:

--------------------------
Application Schema Type
--------------------------

AIM supports two schema types:

- Type 0/2 (default/default when a transformation function is supplied): Indexes existing data only.
- Type 4/6: Indexes data and generates "" (NULL) entries based on a comparison of the subscripts to be indexed versus subscripts based on sub-trees and potentially in another global variable.

See :ref:`record specification <recspec>` for further discussion of type 4/6.

Type 1/3 were an earlier iteration of the same idea behind 4/6, but they were specific to Fileman globals. Type 4/6 handle all Type 1/3 cases, and if the user supplies 1/3, AIM treats them as types 4/6.

.. _forcing:

------------------------
Forcing String Collation
------------------------

In AIM cross reference globals, the cross referenced application data are subscripts. This means that the cross references are ordered by M subscript collation: the empty string, followed by `canonic numbers <../MultiLangProgGuide/programmingnotes.html#canonical-numbers>`_, followed by other strings. While this is appropriate for the majority of applications, for applications whose data can include canonic numbers and strings, cross references should be ordered as strings. Examples include United States zip codes and international telephone numbers, e.g., with default collation, the M code:

.. code::

   YDB>set zip(1)="01801",zip(2)="19355",xref=$$UNXREFDATA^%YDBAIM("^zip",1,,,,,,,,)

Creates the cross reference:

.. code::

   ^%ydbAIMDxYLWlHuPLdyPGfSMaZdn8B(0,19355,2)=""
   ^%ydbAIMDxYLWlHuPLdyPGfSMaZdn8B(0,"01801",1)=""

which is incorrect, as :code:`01801` should sort before :code:`19355`. Using the :code:`force` parameter:

.. code::

   YDB>set ^zip(1)="01801",^zip(2)="19355",xref=$$XREFDATA^%YDBAIM("^zip",1,,,,,,,,1)

Each cross reference is prefixed with :code:`#` and the nodes are sorted correctly:

.. code::

   ^%ydbAIMDQ2cA8Z4cVwjtYEGFKYXY64(0,"#01801",1)=""
   ^%ydbAIMDQ2cA8Z4cVwjtYEGFKYXY64(0,"#19355",2)=""

Notes:

* Applications using AIM globals, for example, `$ORDER() <../ProgrammersGuide/functions.html#order>`_, `ydb_subscript_next_s() / ydb_subscript_next_st() <../MultiLangProgGuide/cprogram.html#ydb-subscript-next-s-ydb-subscript-next-st>`_ and related functions in other languages should remove the leading :code:`"#"` from the subscripts reported by AIM when traversing application globals, and prepend a leading :code:`"#"` to locate cross referenced data.

* YottaDB recommends using 1 as the :code:`force` parameter for forcing string collation, to allow other values to be used for other types of forcing.

.. _transformation:

------------------------
Transformation Functions
------------------------

The most common use of cross reference is to find global nodes that contain the data being cross referenced, for example to traverse that data in order. But cross references are useful for many reasons. For example:

* There are multiple formats for storing dates and times, and comparing values directly can slow queries. But if the cross reference for each time stamp is its `UNIX time <https://en.wikipedia.org/wiki/Unix_time>`_ (i.e., its `$ZUT <../ProgrammersGuide/isv.html#zut>`_ value), then comparing time stamps, or choosing dates and times within a range becomes a much simpler proposition. This is a 1:1 transformation function.
* Cross referencing a hash or checksum allows an application to locate the original data for the hash or checksum. This is potentially a many:1 transformation function.

Transformation is accomplished by providing the M code for a function in the :code:`force` parameter with a value of 2 or 6 for the :code:`type` parameter. For example, if :code:`"$$ABC^DEF()"` is the value passed in :code:`force`, triggers for cross referenced nodes will use the value returned by the transformation function as the value to cross reference. When the function is called at runtime by the trigger, the first parameter is the actual node or piece value, e.g., :code:`$$ABC^DEF("2024-02-21T13:31:48.05098021+07:00")` would yield the actual cross-referenced value if the timestamp in the global node is :code:`2024-02-21T13:31:48.05098021+07:00`. If the function requires additional parameters, they can be specified as comma separated values for the second and subsequent parameters, e.g., :code:`"$$ABC^DEF(,1,""two"")"`. As local variables cannot be passed to triggers, these additional parameters can only be constants, global variable references, or function calls whose parameters are constants, global variables, or function calls. Application code that needs to pass local variable values to the transformation function should use `$ZTWORMHOLE <../ProgrammersGuide/isv.html#ztwormhole-isv>`_.

For example, with the ^USPresidents global variable mentioned earlier, the node :code:`^USPresidents(1797,1801)="John||Adams"` would generate the cross reference :code:`^%ydbAIMDHgTwbHgcmyZEIfADw7Xq07(3,"0x5d156e592ad2e9a83eb48043c59213d0",1797,1801)=""` with a call to :code:`$$XREFDATA^%YDBAIM("^USPresidents",2,"|",3,0,0,0,0,2,"$ZYHASH()")`.

A value of 2 for :code:`type`, informs AIM that the schema for the global nodes is an ordinary schema; a value of 6, that the global nodes have a :ref:`record specification <recspec>`.

.. _comment:

------------
Comments
------------

Since an application can have many cross-references, to facilitate connecting a cross reference with the motivation for that cross reference, the :ref:`xrefdata` and :ref:`xrefsub` functions have an optional ``comment`` parameter. Any value passed to the function is stored in the metadata about the cross reference.

.. _recspec:

-------------------------------------
Record Specifications (types 4 and 6)
-------------------------------------

The need for types 4/6 came originally from the need to provide cross-references for applications that needed to map their global schema to relational tables for Octo.

In AIM, a **record** global is a global variable that may differ from the global variable being cross referenced, the **value** global, with which it shares subscripts.

Say the values are in ``^value(i)``, but what defines that a record exists is ``^record(i)``, where ``i`` is common to ``^value`` and ``^record``.

 * If ``^record(i)`` exists but the same ``i`` at ``^value(i)`` does not exist, then the empty string should be used as the cross reference for ``i``.
 * If ``^record(i)`` does not exist, but ``^value(i)`` exists, there should be no cross reference.

Type 0 can only represent the data it sees in ``^value(i)``: it misses the "" entries and adds an extra entry to the cross reference, both of which are incorrect.

Here is a fully specified example:

.. code:: none

   ^G(1)=""           ^G is the "record" global, holding records 1 and 2
   ^G(2)=""
   ^H(1)="Alice"      ^H is the value global (the target of an AIM cross-reference); a value for record 1,
   ^H(3)="Bob"        and one for a record that does not exist

Both type 0 and type 4 will have cross references of :code:`(0,"Alice",1)`. However, the common type 0 will have a cross reference of :code:`(0,"Bob",3)` whereas type 4 will have a cross reference of :code:`(0,"",2)` - see the call to :ref:`xrefdata` below for the latter.

With a value of 4 for **type**, :ref:`xrefdata` takes a **record specification** as well, in the **recgbl** and **recsub** parameters:

.. code:: bash

   YDB>set xsub(1)=":",recsub(1)=":",recsub=1
   YDB>set x=$$XREFDATA^%YDBAIM("^H",.xsub,,,,,,,4,,,"^G",.recsub)
   YDB>zwrite @x@(0,*)
   ^%ydbAIMD...(0,"",2)=""
   ^%ydbAIMD...(0,"Alice",1)=""

Record 2 is cross referenced under the empty string, and record 3 is not cross referenced.

**recgbl** is the global variable in which records live. It defaults to **gbl** if not supplied; it may name a different global variable, even an :ref:`extended reference <extref>` to a global variable in different database.

**recsub** uses the syntax of **xsub**:

* :code:`":"`, optionally a range such as :code:`"1:1900"`, is a **record subscript**. The i-th :code:`":"` in **recsub** is the same subscript as the i-th :code:`":"` in **xsub**. The subscripts of the value specification must be a left prefix of those of the record specification: the value may be keyed by fewer subscripts than the record, e.g. :code:`^H(A)` vs :code:`^G(A,B)`.
* A constant pins a fixed subscript, e.g., the 0 of :code:`^G(A,B,0)`.
* The value at the root of **recsub** is the **depth** of the record specification. This is a tricky concept but is needed for triggers to work properly: If a record is created at ``^G(A,B,C,D,E,F)``, since ``^G(A)`` exists, it should count for ``^H(A)``; but AIM cannot monitor every single level for a trigger due to the way that triggers work (triggers for ``^G(A)`` don't know about ``^G(A,B,C,D,E,F)``), so the depth tells AIM how many levels below ``^G(A)`` to watch: **recsub** would be 6 in this example.

Here are various examples for **recsub**:

.. code:: none

   recsub(1)=":",recsub=1                             a record is ^G(A), and nothing below it
   recsub(1)=":",recsub=2                             a record is ^G(A), cross referenced while any ^G(A,x) exists
   recsub(1)=":",recsub(2)=":",recsub(3)=0,recsub=3   a record is ^G(A,B,0)
   recsub(1)=1,recsub(2)=":",recsub=3                 a record is ^G(1,B), cross referenced while any ^G(1,B,x) exists

Here is another example where the value global and the record global are the same: Employee records where the name is on one node and the e-mail address, which not every employee has, one level down:

.. code:: none

   ^EMP(1,0)="Alice^Cardiology"
   ^EMP(1,0,"EMAIL")="alice@hosp.org"
   ^EMP(2,0)="Bob^Cardiology"

A record is :code:`^EMP(A)`, and the e-mail address lives two levels below it, so the record specification is :code:`recsub(1)=":"` with a depth of 3. Cross referencing the e-mail address:

.. code:: bash

   YDB>set xsub(1)=":",xsub(2)=0,xsub(3)="""EMAIL""",recsub(1)=":",recsub=3
   YDB>set x=$$XREFDATA^%YDBAIM("^EMP",.xsub,,,,,,,4,,,,.recsub)
   YDB>zwrite @x@(0,*)
   ^%ydbAIMD...(0,"",2)=""
   ^%ydbAIMD...(0,"alice@hosp.org",1)=""

If **type** is 4 and neither **recgbl** nor **recsub** is supplied, the value specification serves as the record specification: a record is the value node itself, with nothing below it, and the cross reference holds what the default type holds. A **recgbl** other than **gbl** must come with a **recsub**.

A value of 6 for **type** is type 4 with a :ref:`transformation function <transformation>` in **force**, as type 2 is to type 0. A record whose value node does not exist is then cross referenced under the transformation function applied to the empty string, :code:`f("")`. For example, to cross reference the e-mail addresses of the ``^EMP`` example in lower case, whatever case they are stored in:

.. code:: bash

   YDB>set x=$$XREFDATA^%YDBAIM("^EMP",.xsub,,,,,,,6,"$ZCONVERT(,""L"")",,,.recsub)
   YDB>zwrite @x@(0,*)
   ^%ydbAIMD...(0,"",2)=""
   ^%ydbAIMD...(0,"alice@hosp.org",1)=""
   YDB>set ^EMP(2,0,"EMAIL")="BOB@hosp.org"
   YDB>zwrite @x@(0,*)
   ^%ydbAIMD...(0,"alice@hosp.org",1)=""
   ^%ydbAIMD...(0,"bob@hosp.org",2)=""

Employee 2's entry stays the empty string, as :code:`$ZCONVERT("","L")` is :code:`""`.

The following errors are specific to record specifications:

* ``RECSPEC``: **recgbl** or **recsub** given with a **type** other than 4 or 6; or **type** 4 with a **recgbl** other than **gbl** and no **recsub**.
* ``RECPREFIX``: the value specification has more record subscripts than the record specification.
* ``RECWILD``: a :code:`":"` subscript of **recsub** below a level that is left unnamed.
* ``RECDEPTH``: the depth is not an integer from 1 through 29, or is less than the deepest named subscript of **recsub**.

------------
Functions
------------

.. _xrefdata:

+++++++++++
XREFDATA()
+++++++++++

XREFDATA() computes and maintains cross references for nodes values or pieces of node values, of a global variable at a specified subscript level.

The format for XREFDATA() is as follows:

.. code:: none

  $$XREFDATA^%YDBAIM(gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type,force,comment,recgbl,recsub)

where,

* **gbl** is the global variable name, e.g., ^ABC

* **xsub** is a specification of the subscripts to be cross referenced. There are three cases:

    * xsub has a positive integer value at the root, and no subtrees (i.e., $DATA(xsub) is 1): The value specifies the level (number of subscripts) of the global variable for which the cross reference is to be created, with all subscripts at each level to be included in the cross reference. In this case, the actual parameter can be a literal or a variable passed by value. In other cases it must be passed by reference.
    * xsub has no value at the root, but positive integer subscripts (1), (2), (3), etc (i.e., $DATA(xsub) is 10): The subscripts of the local variable specify the values of the global variable subscript to be cross referenced, using the same syntax as trigvn field of trigger definitions. The last subscript defines the level of the global variable to be cross referenced. Any omitted intervening subscript (e.g., if the local variable has nodes (1) and (3) but not (2)) means that all subscripts at that level should be included in the cross reference.
    * xsub has both a value at the root, as well as positive integer subscripts (i.e., $DATA(xsub) is 11): This is similar to the previous case, except that if the value at the root exceeds the last subscript, that is the level of the global variable to be cross referenced. For example, if the local variable has nodes (1) and (3) but the value at the root is 5, five subscripts of the global variable will be cross referenced. A value at the root smaller than the last subscript is ignored, so with the subscripts above and a value of 2 at the root, three subscripts will be cross referenced.

  Other cases (e.g., non integer subscripts of xsub) raise errors.

* **sep** is the piece separator for values at that node; if unspecified or the empty string, the cross reference is for entire node values.

* **pnum** is a semi-colon separated list of integer piece numbers for which cross references should exist; ignored for cross references of entire node values, effectively a no-op if pieces specified are already cross-referenced.

* **nmonly**, if 1, means just return the cross reference global variable name but don't set triggers or compute cross references.

* **zpiece**, if 1, means that $ZPIECE() should be used as the piece separator instead of $PIECE(). AIM can have cross references for the same nodes with both options; the cross references are in different global variables.

* **omitfix**, if 1, instructs XREFDATA() to omit from the subscripts of the cross reference any subscripts of the application global that are fixed constants because the code to traverse the application global using the cross reference will include those known fixed subscripts when making the access. If not specified, omitfix defaults to 1.

* **stat** if 1 or 2 says the metadata should include statistics, as described above under :ref:`statistics`.

* **type**, defaulting to the empty string, specifies the application schema for which AIM is being asked to compute and maintain metadata; see :ref:`schematype` and :ref:`recspec`.

* **force**, defaults to the empty string. A value of 1 specifies that AIM cross references should prepend a hash (:code:`#`) to the data being cross referenced. If **type** is 2 or 6, **force** specifies a transformation function. See :ref:`forcing`.

* **comment**, if any value is passed by the caller, results in that value stored in the metadata of the cross reference, under the piece number when **sep** is specified, since several pieces of one node share a cross reference and each may be described separately.

* **recgbl** and **recsub**, for **type** 4 and 6, are the :ref:`record specification <recspec>`: the global variable in which records live, defaulting to **gbl**, and the subscripts that locate a record, in the syntax of **xsub** with the depth at the root.

The relationship between **type** and **force** is shown below. Types 1 and 3 are accepted as 4 and 6, as described under :ref:`schematype`. Combinations of values other than those shown are reserved.

+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
|                                                                           **type**                                                                           |
+-------------------------------+----------------------------+-------------------------+-----------------------------------+-----------------------------------+
|                               |   **0, "", or omitted**    |          **2**          |               **4**               |               **6**               |
+===========+===================+============================+=========================+===================================+===================================+
|           | **0, "", or**     | Normal schema;             |                         | Record specification;             |                                   |
|           | **omitted**       | normal M ordering          |                         | normal M ordering                 |                                   |
|           +-------------------+----------------------------+                         +-----------------------------------+                                   |
| **force** |       **1**       | Normal schema;             |                         | Record specification;             |                                   |
|           |                   | forced string ordering     |                         | forced string ordering            |                                   |
|           +-------------------+----------------------------+-------------------------+-----------------------------------+-----------------------------------+
|           |                   |                            | Normal schema;          |                                   | Record specification;             |
|           |    **Function**   |                            | transformation function |                                   | transformation function           |
|           |                   |                            | ordering                |                                   | ordering                          |
+-----------+-------------------+----------------------------+-------------------------+-----------------------------------+-----------------------------------+

Metadata about the cross reference itself is stored in nodes of the cross-reference global variable as follows:

*  The root node is the application global variable name (the **gbl** parameter of the function call).

*  Subscripted nodes are:

  *  \(0) space separated `$ZUT <../ProgrammersGuide/isv.html#zut>`_ of the time the cross reference was completed, `$JOB <../ProgrammersGuide/isv.html#job>`_ of the process, `$ZYRELEASE <../ProgrammersGuide/isv.html#zyrelease-isv>`_ of the YottaDB release number and, a format version number for the metadata.
  *  \(1) number of cross-referenced subscripts of the application global variable.
  *  \(2) piece separator, if any. The empty string specifies whole-node cross references.
  *  \(3) \& \(4) piece numbers, in the form of a bit-map like string, prefixed with ``"#"`` to prevent numeric conversion, e.g., the value for pieces 2, 4 and 5 would be ``"#01011"``, the empty string for an cross reference of the entire node. The (3) node identifies the piece numbers for which cross referencing is complete whereas (4) identifies those for which triggers exist. If they are not equal, it means that a process created triggers, but is still working on cross referencing existing global nodes. It is also possible that the process terminated, or was terminated, before completing its work.
  *  \(5) 1 means $ZPIECE() was used for pieces; the default empty string is $PIECE().
  *  \(6) SET trigger for this cross reference.
  *  \(7) KILL trigger for this cross reference; see also comment for (12) below.
  *  \(8) ZKILL trigger for this cross reference.
  *  \(9) 1 means that omitting fixed subscripts was requested, whether or not any subscripts were actually omitted.
  *  \(10) if 1 or 2 means that statistics are maintained, as specified by the ``stat`` parameter.
  *  \(11) stores any value that is passed in the ``comment`` parameter; \(11,piece) when **sep** is specified.
  *  \(12) & up - triggers for KILLs of higher level nodes.
  *  \(51) & \(53) & up - for types 4 and 6, triggers for KILLs of the record global variable and of the nodes above the record node, where these are not already among the triggers above.
  *  \(99) for types 4 and 6, the global directory of the database holding records, present only when that is not the database of the cross reference.
  *  \(100+i) & \(140+i) for types 4 and 6, the SET and ZKILL triggers of record level i, one pair for the record node and for each watched level below it.

Applications can read and use the above metadata, but should not attempt to alter it. Changes can result in unpredictable and/or undesirable behavior.

.. _xrefsub:

++++++++++
XREFSUB()
++++++++++

XREFSUB() computes and maintains cross references for subscripts of a global variable at specified subscript levels.

The format for XREFSUB() is as follows:

.. code:: none

   XREFSUB(gbl,xsub,snum,nmonly,omitfix,stat,type,force,comment)

where

* **gbl**, **xsub**, **nmonly**, **omitfix**, and **stat** are the same as for XREFDATA().

* **snum** uses the same syntax as **pnum** for XREFDATA() to specify subscripts that should be cross-referenced. Unlike **pnum**, it is not optional. Any subscript that is specified by **snum** must be part of **xsub**, e.g., if **snum** specifies cross referencing of the third subscript, **xsub** must specify a global variable with at least 3 subscripts.

* **type** defaults to the empty string, which is also equivalent to 0. A value of 2 indicates that **force** is a transformation function. XREFSUB() has no record specification.

* **force** defaults to the empty string, which is equivalent to 0. A value of 1 when **type** is defaulted or zero indicates that AIM cross references should prepend a hash (``#``) to the cross reference, which forces string collation even for numbers (e.g., US zip codes). If **type** is 2, **force** should be a string specifying a transformation function for cross references (see :ref:`forcing`).

* **comment**, if any value is passed by the caller, results in that value stored in the metadata of the cross reference.

The relationship between **type** and **force** is shown below. Combinations of values other than those shown are reserved.

+--------------------------------------------------------------------------------------+
| **type**                                                                             |
+-------------------------------+----------------------------+-------------------------+
|                               | **0, "", or omitted**      |         **2**           |
+===========+===================+============================+=========================+
|           | **0, "", or**     | Normal schema;             |                         |
|           | **omitted**       | normal M ordering          |                         |
|           +-------------------+----------------------------+                         |
|           |       **1**       | Normal schema;             |                         |
| **force** |                   | forced String ordering     |                         |
|           +-------------------+----------------------------+-------------------------+
|           |                   |                            | Normal schema           |
|           |  **Function**     |                            | transformation function |
|           |                   |                            | ordering                |
+-----------+-------------------+----------------------------+-------------------------+

Metadata about the cross reference itself is stored in nodes of the cross-reference global variable as follows:

*  The root node is the application global variable name (the **gbl** parameter of the function call).

*  Subscripted nodes are:

  *  \(0) space separated `$ZUT <../ProgrammersGuide/isv.html#zut>`_ of the time the cross reference was completed, `$JOB <../ProgrammersGuide/isv.html#job>`_ of the process, `$ZYRELEASE <../ProgrammersGuide/isv.html#zyrelease-isv>`_ of the YottaDB release number and, a format version number for the metadata.
  *  \(1) number of cross-referenced subscripts of the application global variable.
  *  \(2) unused, but reserved.
  *  \(3) \& \(4) subscript numbers, in the form of bit strings manipulated by `$ZBIT*() <../ProgrammersGuide/functions.html#zbit-functions>`_ functions. The (3) node identifies the piece numbers for which cross referencing is complete whereas (4) identifies those for which triggers exist. If they are not equal, it means that a process created triggers, but is still working on cross referencing existing global nodes. It is also possible that the process terminated, or was terminated, before completing its work.
  *  \(5) unused but reserved.
  *  \(6) SET trigger for this cross reference.
  *  \(7) KILL trigger for this cross reference; see also comment for (12) below.
  *  \(8) ZKILL trigger for this cross reference.
  *  \(9) 1 means that omitting fixed subscripts was requested, whether or not any subscripts were actually omitted.
  *  \(10) if 1 or 2 means that statistics are maintained, as specified by the ``stat`` parameter.
  *  \(11) stores any value that is passed in the ``comment`` parameter.
  *  \(12) & up - triggers for KILLs of higher level nodes.

Applications can read and use the above metadata, but should not attempt to alter it. Changes can result in unpredictable and/or undesirable behavior.

.. _lsxref:

+++++++++++++++++++++++++++
LSXREFDATA() / LSXREFSUB()
+++++++++++++++++++++++++++

LSXREFDATA() lists metadata for a data cross reference, all data cross references for a global variable, or all data cross references. LSXREFSUB() lists metadata for a subscript cross reference, all subscript cross references for a global variable, or all subscript cross references.

The format for LSXREFDATA() / LSXREFSUB() is as follows:

 .. code:: none

   DO LSXREFDATA^%YDBAIM(lvn[,gbl])
   DO LSXREFSUB^%YDBAIM(lvn[,gbl])

where,

* **lvn** is a local variable passed by reference. In that local variable, the function describes all cross references as follows:

    * The first subscript is the cross reference global variable name, e.g., :code:`^%ydbAIMDgBPWsnL76HLyVnlvsrvE19` or ``^%ydbAIMSrNrMckj7LkdFXjsHkuT91D``. The value of its root node (i.e., with no subscript) is the application global variable name, e.g., :code:`^USPresidents`.
    * Nodes with positive integer second subscripts have metadata about the metadata. These are described in :ref:`xrefdata` and :ref:`xrefsub`.

  Nodes of lvn other than those corresponding to reported cross references remain unaltered.

* **gbl** is a global variable name. There are three cases:

    * It is an application global variable name, e.g., :code:`^USPresidents`. In ``lvn`` as described above, the function returns all data or subscript cross references for that global variable, depending on whether the function is LSXREFDATA() or LSXREFSUB().
    * It starts with :code:`^%ydbAIMD` or ``^%ydbAIMS``. In ``lvn``, the function returns information about the specified cross reference.
    * It is omitted or the empty string (""). In ``lvn``, the function returns information about all data or subscript cross references, depending on whether the function called is LSXREFDATA() or LSXREFSUB().

.. _unxref:

+++++++++++++++++++++++++++
UNXREFDATA() / UNXREFSUB()
+++++++++++++++++++++++++++

* UNXREFDATA() removes all data metadata; UNXREFSUB() removes all subscript metadata
* UNXREFDATA(aimgbl) and UNXREFSUB(aimgbl) where aimgbl is an AIM metadata global variable, removes the metadata stored in that AIM global. Note that an AIM global variable stores either data metadata or subscript metadata, but not both.
* UNXREFDATA(gbl) where gbl is an application global name removes all data metadata for that application global; UNXREFSUB(gbl) does likewise for subscript metadata.

UNXREFDATA() and UNXREFSUB() provide an API for removing specific cross references. The APIs mirror those of :ref:`xrefdata` and :ref:`xrefsub`, allowing for removal of a specific cross reference by replicating the parameters used to call the functions that created the cross references in the first place. The APIs are as follows and the parameters are described in :ref:`xrefdata` and :ref:`xrefsub`. Note that some parameters are specific to one function or the other.

 .. code:: none

   DO UNXREFDATA^%YDBAIM(gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type,force,recgbl,recsub)
   DO UNXREFSUB^%YDBAIM(gbl,xsub,snum,nmonly,omitfix,stat,type,force)

where,

* **gbl** is the global variable name, e.g., :code:`^ABC` for which the metadata is to be removed. If omitted, all cross references and triggers for cross references are removed as discussed above.

* **xsub** is a specification of the subscripts in the cross reference to be removed.

* **sep** is the piece separator for values at that node; if unspecified or the empty string, the cross reference signature is entire node values.

* **pnum** and **snum** exists to allow the parameters of UNXREFDATA() and UNXREFSUB() to match those of :ref:`xrefdata` and :ref:`xrefsub` and are ignored. Note that regardless of the values of these parameters, calling the functions removes the metadata for all pieces, and all subscripts.

* **nmonly** is ignored.

* **zpiece**, must match that of the :ref:`xrefdata` call that set up the cross reference.

* **omitfix** and **stat** are ignored.

* **type** and **force** must match the :ref:`xrefdata` or :ref:`xrefsub` that set up the cross reference.

* **recgbl** and **recsub**, for types 4 and 6, must match the :ref:`record specification <recspec>` given to :ref:`xrefdata`.

++++++++++
VERSION()
++++++++++

VERSION() provides `semantic version <https://semver.org/>`_ information for AIM, consisting of a major version number and a minor version number, separated by a period.

* A change to the major version number indicates a breaking change, which for AIM primarily means a change to the metadata schema. There are separate major version numbers for data and subscript metadata.
* A change to the minor version number indicates an upward-compatible (non-breaking) change. The minor version number is common to data and subscript metadata.

There is one optional parameter, as follows:

* VERSION(str) where ``str`` is case-insensitive ``"DATA"`` or ``"SUB"`` reports the major version number for the selected type of metadata, as well as the minor version number.
* VERSION() reports the major version as the sum of the data and subscript metadata, as well as the minor version number.

For example:

.. code:: none

   YDB>write $$VERSION^%YDBAIM
   7.3
   YDB>

.. note::
   The version numbers shown above are illustrative; the values your installation reports may differ as AIM evolves.

.. _extref:

----------------------------
Extended References
----------------------------

If **gbl** is an extended reference, the cross reference variable returned by :ref:`xrefdata` and :ref:`xrefsub` is an extended reference using the same global directory as **gbl**. The triggers that maintain the cross references also use the extended reference, since it is not known at the time that the functions are called whether a future update to the global variable will use an extended reference or set `$ZGBLDIR <../ProgrammersGuide/isv.html#zgbldir>`_. Using an extended reference in the trigger ensures that the application global variable and cross reference use the same global directory.

:ref:`lsxref`, and :ref:`unxref` all accept extended references.

For types 4 and 6, **recgbl** may be an extended reference, placing records in a database other than that of **gbl** and the cross reference. The triggers that maintain the record side of the cross reference are then defined in that database, and name the cross reference with its own database. The cross references reported by the first two functions do not include the global directory name, i.e., they are reported as normal global variables and not as extended references.

.. note::
   As using extended references has a small incremental performance cost on global variable accesses, define the cross reference using an extended reference only if the application global variable will normally be accessed by the application using an extended reference.

----------------------------
Environment Variables
----------------------------

.. _ydb_aim_nproc:

++++++++++++++++
ydb_aim_nproc
++++++++++++++++

:code:`ydb_aim_nproc` is the number of processes :ref:`xrefdata` and :ref:`xrefsub` use to scan a global variable for the nodes that exist when a cross reference is created. It must be a positive integer; any other value raises an ``INVNPROC`` error. If it is not set, the default is one less than the number of online CPUs, and one on a single CPU machine. The process that calls XREFDATA() or XREFSUB() is not idle while the others scan: it walks a subscript level to divide the work among them, so the default leaves a CPU for it.

Since the processes contend with one another for the blocks of the cross reference they are all updating, as well as with whatever else is running on the system, a value smaller than the number of CPUs may complete the scan sooner than a value equal to it; measure rather than assume.

:code:`ydb_aim_nproc` affects only how long the initial scan takes, and neither the cross reference produced nor its name. It is of no consequence when a cross reference is created for a global variable with no existing nodes, since there is nothing to scan.

The scan divides the global variable at one subscript level: the outermost level whose specification in **xsub** is not a constant, skipping past any such level that holds just one subscript. The number of processes that can be kept busy is therefore limited by the number of distinct subscripts at the level chosen. A specification whose outermost such level has few subscripts, with the bulk of the global variable below it, does not benefit from a large :code:`ydb_aim_nproc`.

----------------------------
Operational Considerations
----------------------------

* Any region to which :code:`^%ydbAIM*` global variables are mapped should have NULL_SUBSCRIPTS set to `ALWAYS <../AdminOpsGuide/gde.html#no-n-ull-subscripts-always-never-existing>`_, and implement `standard null collation <../AdminOpsGuide/gde.html#no-std-nullcoll>`_.
* YottaDB recommends setting journaling and replication to the YDBAIM region to match the settings of the application database region(s) that AIM cross references. This is because AIM sets `triggers <../ProgrammersGuide/triggers.html#triggers>`_ in those regions to maintain AIM metadata in sync with application data.
* If the YDBAIM region and application data become out of sync with each other, use UNXREFDATA() followed by XREFDATA() to resynchronize them.
* If `ydb-treat-sigusr2-like-sigusr1 <../AdminOpsGuide/basicops.html#ydb-treat-sigusr2-like-sigusr1>`_ is set, on receipt of a SIGUSR2, %YDBAIM terminates indexing of data and returns to its caller; otherwise it ignores SIGUSR2. This facilitates use of %YDBAIM by Octo.

.. raw:: html

    <img referrerpolicy="no-referrer-when-downgrade" src="https://download.yottadb.com/plugins.png" />
