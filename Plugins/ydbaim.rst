.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.#
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

All applications need metadata. By providing an API to manage metadata, and enable content-based access to data, YDB AIM (Application Independent Metadata) removes the need for each YottaDB application to create its own separate framework.

.. _quickstart:

-------------
Quickstart
-------------

As a YottaDB plugin, AIM requires YottaDB.

Install YottaDB and AIM together:

.. code:: bash

   mkdir /tmp/tmp ; wget -P /tmp/tmp https://gitlab.com/YottaDB/DB/YDB/raw/master/sr_unix/ydbinstall.sh
   cd /tmp/tmp ; chmod +x ydbinstall.sh
   sudo ./ydbinstall.sh --utf8 default --verbose --aim

If you already have YottaDB installed, use the :code:`--plugins-only` option to install specified plugins.

-------------
Installation
-------------

If you don't use the :ref:`quickstart` method, you can install YDB AIM from source.

To install, you need :code:`cmake`, :code:`cc`, and :code:`ld` commands.

Download the YDBAIM repository, and then install, using the following commands:

.. code:: bash

   git clone https://gitlab.com/YottaDB/Util/YDBAIM.git YDBAIM-master
   cd YDBAIM-master
   sudo -E ./install.sh

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

Since the first President Adams record does not include a middle name, the corresponding record has an empty string ("") subscript. *Any region to which ^%ydbAIMD\* global variables are mapped should have NULL_SUBSCRIPTS set to ALWAYS.* Since the subscripts will include pieces of global nodes, or even entire global nodes, it would be prudent to set YottaDB's maximum key size (1019 bytes) for that region.

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

.. _statistics:

+++++++++++++
Statistics
+++++++++++++

The optional parameter :code:`stat` can be used to instruct AIM that the application wishes to compute and maintain statistics. There are two types of statistics (the default, stat=0, is cross references only and no statistics):

* **stat=1**: statistics on the count of each value. Thus the call :code:`$$XREFDATA^%YDBAIM("^USPresidents",2,"|","1:3",,,,1)` would compute and maintain nodes such as :code:`^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-1,"John")=4` to indicate that "John" appears as the first piece four times (the first subscript is the negative of the piece number).

* **stat=2**: in addition to the count of each value, also counts the number of different values, and also a total count of the number of values maintained. Thus, the call :code:`$$XREFDATA^%YDBAIM("^USPresidents",2,"|","1:3",,,,2)` would compute and maintain nodes such as :code:`^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-3)=39` to indicate that there are 39 distinct last names and :code:`^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(11)=135` to indicate that there are 135 nodes maintained (as of 2021, the 45 former US Presidents times 3 names for each ex-President).

~~~~~~~~~
Example
~~~~~~~~~

.. code:: bash

   YDB> s x=$$XREFDATA^%YDBAIM("^USPresidents",2,"|","1:3",,,,2)

   YDB> s z="" f i=1:1:5  s z=$o(@x@(-1,z)) zwr @x@(-1,z) ; count of each value
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-1,"Abraham")=1
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-1,"Andrew")=2
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-1,"Barack")=1
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-1,"Benjamin")=1
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-1,"Bill")=1

   YDB> zwr %ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-3:-1),^(11) ; number of distinct values and number of total values
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-3)=39 ; 39 distinct last names
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-2)=14 ; 14 distinct middle names/initials
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-1)=31 ; 31 distinct first names
   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(11)=135 ; a total of 135 nodes maintained

   YDB>

Statistics can be used to optimize queries. For example, if one wants a query where the first name is George (:code:`^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-1,"George")=3`), middle initial is W. (:code:`^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-2,"W.")=1`) and the last name is Bush (:code:`^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(-3,"Bush")=2`), it would make sense to look at the middle initial first (since its count is one). This approach minimizes the number of global accesses.

Consider another example, a global variable that contains all of the U.S. State Capitals and you would like to find out which capitals are named after the Presidents. In this case it would make sense to look at the last names of the Presidents first, since there are 39 distinct last names and 50 distinct state capitals.

--------------------------
Application Schema Type
--------------------------

The above description describes the most straightforward type of application schema, where all application nodes with metadata managed by AIM have the same number of subscripts. However, the freedom that global variables provide to application designers means that different applications design their schemas in different ways.

The default type parameter in the call to XREFDATA() creates metadata for straightforward case above. Adding schemas consists of:

* Creating new trigger templates as needed, and creating triggers from new and existing trigger templates.
* Adding logic in XREFDATA() to create the initial metadata.

With a value of 1 for type, AIM creates and manages metadata for a schema used by the `VistA Fileman software <https://www.va.gov/vdl/application.asp?appid=5>`_.

For a type 1 schema, when

* the last subscript specification specifies a constant;
* a node with that constant subscript does not exist; and
* other nodes exist at the level of that constant subscript, i.e., there is at least one other node whose subscripts are identical except for that constant last subscript.

AIM creates and maintains metadata nodes for the requested pieces using the empty string ("") as the last subscript intead of the specified constant.
If omitfix=1 (the default), the metadata omits that last empty string subscript.

Metadata for nodes with that constant subscript that do exist have the same schema as metadata for the default type ("").

------------
Functions
------------

.. _xrefdata:

+++++++++++
XREFDATA()
+++++++++++

XREFDATA() is used to create triggers to maintain cross references and compute cross references for a global variable at a specified subscript level.

The format for XREFDATA() is as follows:

 .. code:: none

   $$XREFDATA^%YDBAIM(gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type)

where,

* **gbl** is the global variable name, e.g., ^ABC

* **xsub** is a specification of the subscripts to be cross referenced. There are three cases:

    * xsub has a positive integer value at the root, and no subtrees (i.e., $DATA(xsub) is 1): The value specifies the level (number of subscripts) of the global variable for which the cross reference is to be created, with all subscripts at each level to be included in the cross reference. In this case, the actual parameter can be a literal or a variable passed by value. In other cases it must be passed by reference.
    * xsub has no value at the root, but positive integer subscripts (1), (2), (3), etc (i.e., $DATA(xsub) is 10): The subscripts of the local variable specify the values of the global variable subscript to be cross referenced, using the same syntax as trigvn field of trigger definitions. The last subscript defines the level of the global variable to be cross referenced. Any omitted intervening subscript (e.g., if the local variable has nodes (1) and (3) but not (2)) means that all subscripts at that level should be included in the cross reference.
    * xsub has both a value at the root, as well as positive integer subscripts (i.e., $DATA(xsub) is 11): This is similar to the previous case, except that if the value at the root exceeds the last subscript, that is the level of the global variable to be cross referenced. For example, if the local variable has nodes (1) and (3) but the value at the root is 5, five subscripts of the global variable will be cross referenced. A value at the root smaller than the last subscript is ignored, so with the subscripts above and a value of 2 at the root, three subscripts will be cross referenced.

  Other cases (e.g., non integer subscripts of xsub) raise errors.

* **sep** is the piece separator for values at that node; if unspecified or the empty string, the cross reference is for entire node values.

* **pnum** is a semi-colon separated list of integer piece numbers for which cross references should exist; ignored for xrefs of entire node values, effectively a no-op if pieces specified are already cross-referenced.

* **nmonly**, if 1, means just return the xref global variable name but don't set triggers or compute xrefs.

* **zpiece**, if 1, means that $ZPIECE() should be used as the piece separator instead of $PIECE(). AIM can have cross references for the same nodes with both options; the cross references are in different global variables.

* **omitfix**, if 1, instructs XREFDATA() to omit from the subscripts of the cross reference any subscripts of the application global that are fixed constants because the code to traverse the application global using the cross reference will include those known fixed subscripts when making the access. If not specified, omitfix defaults to 1.

* **stat** if 1 or 2 says the metadata should include statistics, as described above under :ref:`statistics`.

* **type**, defaulting to the empty string, specifies the application schema for which AIM is being asked to compute and maintain metadata.

+++++++++++++
LSXREFDATA()
+++++++++++++

LSXREFDATA() lists metadata for a cross reference, all cross references for a global variable, or all cross references.

The format for LSXREFDATA() is as follows:

 .. code:: none

   DO LSXREFDATA^%YDBAIM(lvn[,gbl])

where,

* **lvn** is a local variable passed by reference. In that local variable, the function describes all cross references as follows:

    * The first subscript is the cross reference global variable name, e.g., :code:`^%ydbAIMDgBPWsnL76HLyVnlvsrvE19`. The value of that node (i.e., with a first subscript and no second subscript) is the application global variable name, e.g., :code:`^xyz`.
    * Nodes with positive integer second subscripts have metadata about the metadata. These are described in :ref:`xrefdata`.

  Nodes of lvn other than those corresponding to reported cross references remain unaltered.

* **gbl** is a global variable name. There are three cases:

    * It is an application global variable name, e.g., :code:`^USPresidents`. In lvn as described above, the function returns all cross references for that global variable.
    * It starts with :code:`^%ydbAIMD`. In lvn, the function returns information about the specified cross reference.
    * It is omitted or the empty string (""). In lvn, the function returns information about all cross references.

+++++++++++++
UNXREFDATA()
+++++++++++++

* UNXREFDATA() deletes all metadata
* UNXREFDATA(gbl) where gbl is an application global name deletes all AIM metadata for that application global.
* UNXREFDATA(aimgbl) where aimgbl is an AIM metadata global variable, removes that metadata.

The format for UNXREFDATA() is as follows:

 .. code:: none

   DO UNXREFDATA^%YDBAIM(gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type)

where,

* **gbl** is the global variable name, e.g., :code:`^ABC` for which the specified triggers are to be removed. If omitted, all xrefs and triggers for xrefs are removed.

* **xsub** is a specification of the subscripts in the cross reference to be removed. There are four cases:

    * xsub is unspecified or its root node is zero and there is no subtree. In this case, all cross references for the specified global variable are removed. In the following three cases, as the subscript specification is part of the "signature" of a cross reference, the subscript specification of xsub must match that of the trigger being removed.
    * xsub has a positive integer value at the root, and no subtrees ($DATA(xsub) is 1): The value specifies the level (number of subscripts) of the global variable for which the cross reference is to be removed, with all subscripts at each level in the signature of the cross reference. In this case, the actual parameter can be a literal or a variable passed by value. In both the following cases it must be passed by reference.
    * xsub has no value at the root, but positive integer subscripts (1), (2), (3), etc. ($DATA(xsub) is 10): The subscripts of the local variable specify the values of the global variable subscript in the signature cross referenced, using the same syntax as trigvn field of trigger definitions. The last subscript defines the level of the global variable to be cross referenced. Any omitted intervening subscript (e.g., if the local variable has nodes (1) and (3) but not (2)), means that all subscripts at that level are in the cross reference signature.
    * xsub has both a value at the root, as well as positive integer subscripts ($DATA(xsub) is 11): This is similar to the previous case, except that should the number at the root exceed the last subscript, the value at the root is the level of the cross reference signature, with all global variables to be included at the levels beyond those of the last local variable subscript. A value at the root smaller than the last subscript is ignored.

  Other cases (e.g., non integer subscripts of xsub) raise errors.

* **sep** is the piece separator for values at that node; if unspecified or the empty string, the cross reference signature is entire node values.

* **pnum** exists to allow the parameters of UNXREFDATA() to match those of XREFDATA() and is ignored. Note that it is not possible to remove the cross reference of one piece of a node.

* **nmonly** exists to allow the parameters of UNXREFDATA() to match those of XREFDATA() and is ignored.

* **zpiece**, if 1 means that $ZPIECE() was used as the piece separator instead of $PIECE(); this is part of the trigger signature.

* **omitfix** and **stat** exist only to allow the parameters of UNXREFDATA() to match those of XREFDATA() and are ignored.

* **type** is used to get the name of the global, and is optional. If used in the XREFDATA() call, it should be passed here.


----------------------------
Operational Considerations
----------------------------

* Any region to which :code:`^%ydbAIMD*` global variables are mapped should have NULL_SUBSCRIPTS set to ALWAYS.
* Journaling and Replication are not necessary for AIM globals since they can be generated on demand as needed. Of course, you can always turn on journaling and replication if you so choose.
* By default :code:`ydb_env_set` puts the AIM globals in a separate region and they get deleted upon recovering from a crash.
* To avoid tuning and to maximize performance, AIM globals use the MM access method. It directly maps the cross reference into the address space of the process.
