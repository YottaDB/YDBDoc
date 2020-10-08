.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2017-2021 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This source code contains the intellectual property     #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. index::
   GT.CM Client Server

========================
13. GT.CM Client/Server
========================

.. contents::
   :depth: 2

--------------------
Introduction
--------------------

GT.CM is the network client/server database software for YottaDB. GT.CM on UNIX allows access to YottaDB databases residing on a server, by client processes running on multiple nodes on a network.

GT.CM consists of a Server and a Client. The Server is a network object that performs database operations on behalf of YottaDB client processes running on other nodes of the network. GT.CM uses TCP task-to-task communication facilities for the link between a client YottaDB process and a server. GT.CM on UNIX operates properly between supported platforms independent of native byte ordering. The GT.CM client is packaged with the YottaDB run-time system.

When a YottaDB process requires access to a database on another node, it sends a request across the network to a GT.CM Server process running on that node, and the Server accesses the database. The process requesting access to a database through GT.CM is referred to as a Client process. The node from which the data is requested is referred to as the Server node.

The use of GT.CM is largely transparent to the YottaDB application code. The only visible change in the application's environment is the addition of error messages delivered in case of problems in network operations.

GT.CM can communicate between systems having different endian architectures.

.. note::
   YottaDB transaction processing (TP) is not supported via GT.CM, and accessing GT.CM served databases within an M TRANSACTION may cause application level inconsistency in the data. GT.CM servers do not invoke triggers. This means that the client processes must restrict themselves to updates which don't require triggers, or explicitly call for the actions that triggers would otherwise perform. Because GT.CM bypasses triggers, it may provide a mechanism to bypass triggers for debugging or complex corrections to repair data placed in an inconsistent state by a bug in trigger logic.

In the event of recovery from system crashes, application-level database consistency cannot be guaranteed for data residing in databases (M global variable namespaces) accessed via different GT.CM servers or distributed between GT.CM and local access.

---------------------
Overview
---------------------

A YottaDB program uses Global Directory to reference a global variable (gvn) or resource name for the object of a database lock operation (nref) residing on a remote node. When a file in the Global Directory specifies a remote node name that does not match the name of the node on which the process is running, YottaDB maps the segment to a database file on the remote node using the GT.CM client. The two main components of GT.CM are:

* GT.CM Server
* GT.CM Client

+++++++++++++
GT.CM Server
+++++++++++++

The GT.CM server accepts requests from GT.CM clients, processes the operation requested by the clients on the server database and sends messages back to the clients with a status and if appropriate, the results of the requested operation.

.. _gt-cm-client:

+++++++++++++
GT.CM Client
+++++++++++++

The GT.CM client sends messages containing the operation type (SET, KILL, $ORDER, etc), and operation specific data (eg. gvn to be SET, or KILLed) to the GT.CM server through a communication channel over a network. The client generally waits for a response from the server and initiates error processing based on the status contained in the response. The format of the messages exchanged between the server and the client is as defined by the YottaDB developed GNP protocol.

The Global Directory of the client specifies a GT.CM database segment by prefixing its file with an alphanumeric <node-name>, followed by a colon (":"). Client processes using this database must have an environment variable of the form "ydb_cm_<node-name>" to locate the server. This environment variable may contain either a port number alone, or a host name or address and a port number in the form "<host-name-or-address>:<port-number>" or the form "[<host-name-or-address>]:<port-number>", where the square-brackets ([])are part of the text. If the port number is specified alone, GT.CM uses the <node-name> as the host name; otherwise, it uses the <node-name> solely as an identifier to match the segment in the Global Directory, and it obtains the host name or address from the contents of the environment variable. If a host name is specified, and the server host has multiple addresses, GT.CM uses the system default.

.. note::
   Because the <node-name> is strictly alphanumeric, it cannot represent an IP address or qualified host name due to the need for a dot (".") separator. If you need to specify an IP address or qualified host name, select an alphanumeric <node-name> and specify the <host-name-or-address> in the ydb_cm_<node-name> variable. Use the "[<host-name-or-address>]:<port-number>" form to specify an IPv6 address.

++++++++++++++++++++++++++++++++++
GT.CM Server Startup and Shutdown
++++++++++++++++++++++++++++++++++

This section describes the starting up and shutting down procedure of a GT.CM server.

~~~~~~~~~~~~~~~~~~~~
GT.CM Server Startup
~~~~~~~~~~~~~~~~~~~~

A GT.CM server must be operating on every node of the network from which data is requested during distributed database operation, including server nodes and nodes with both client and server processes. There are two ways by which the GT.CM server can be invoked.

* By explicitly starting the GT.CM server to listen to a specified port number, or by defaulting the port number.
* Invoking the GT.CM server to listen at a standard port number assigned to the GNP protocol (e.g., in /etc/services file).

The GT.CM server executable (gtcm_gnp_server) should be placed in the directory referenced by the environment variable $ydb_dist.

A process starting the GT.CM server must have the environment variables required to run YottaDB.

Here is an example on how to start a GT.CM server:

.. code-block:: bash

   $ydb_dist/gtcm_gnp_server -log=GTCM.log -service=6789

This starts the GT.CM server in the background so that it listens at port 6789 for requests from GT.CM clients. The detailed log information of the server is written in the GTCM.log file. If -log is not specified, log information is written in $ydb_log/gtcm_gnp_server.log file. On nodes with multiple IP addresses, issue the following command to configure the GT.CM server to listen at a port specific to an IP address:

.. code-block:: bash

   -service=192.160.105.212:6789

~~~~~~~~~~~~~~~~~~~~~
GT.CM Server Shutdown
~~~~~~~~~~~~~~~~~~~~~

To shutdown the GT.CM server, identify the process id of the GT.CM server to be shutdown and issue the following command:

.. code-block:: bash

   $ydb_dist/mupip stop <GT.CM server PID>

This causes the GT.CM server to shutdown normally.

++++++++++++++++++++++
Types of Operations
++++++++++++++++++++++

The GT.CM client sends messages to the GT.CM server requesting the type of operation to be performed.

GT.CM server can recognize the following types of operations and process the specified operations on the "local" database.

* SET
* KILL
* GET
* DATA
* ORDER
* REVERSE ORDER
* QUERY
* LOCK
* UNLOCK
* ZALLOCATE
* ZDEALLOCATE

The MERGE, SET $PIECE() and SET $EXTRACT() facilities are currently implemented by the client using the operations from the above set.

+++++++++++++++++
Error Messages
+++++++++++++++++

Errors can be classified into the following categories:

* Database Errors
* Protocol Errors
* Session Establishment Errors

Each type of valid operation may issue an error from any of the above categories in case of a failure. Database errors include application errors and database integrity errors; both types of errors are detected by the YottaDB runtime system. The GT.CM server does not deal with database errors directly, but passes them back to the client requesting the operation that detected the error. YottaDB handles any database errors delivered through the network by GT.CM in a way similar to the way it treats errors that are detected when GT.CM is not involved.

When GT.CM is in use, YottaDB may deliver errors resulting from network problems. Errors detected by the network interface are passed to the component accessing the interface at the time of error. In recovering from a network related error, GT.CM sacrifices all LOCKs owned by the client process that receives a network error. This should be taken into account if such a process attempts to resume operations involving a database served through the lost connection.

Examples of Database Errors:

.. code-block:: none

   Undefined global, Global reference content not valid.

Examples of Protocol Errors:

.. code-block:: none

   Message format not valid, Operation type not valid.

Examples of Session Establishment Errors:

.. code-block:: none

   GNP version not supported, GNP session not established.

+++++++++++++++++++
Examples
+++++++++++++++++++

The following is an example illustrating the transparency of the GT.CM Client/Server Architecture while using YottaDB.

**On NODE1**:

Map the local segment to remote file.

When the file specification in the Global Directory on the local node specifies a remote node name, YottaDB maps the segment to a database on the remote node using GT.CM.

To specify a node name in a Global Directory file specification, use the format on NODE1:

.. code-block:: bash

   $ GDE
   GDE> ch -seg DEFAULT -file=NODE2:/testarea/yottadb/database/data.dat
   GDE> exit

This example creates a local Global Directory, mapping all global names to the database file /testarea/yottadb/database/data.dat. Note that some of the key-words have been truncated.

**On NODE2**:

Create a database file on server Node2:

Change directory (cd) to the specified location (that is /testarea/yottadb/database).

Create a global directory.

.. code-block:: bash

   $ GDE
   GDE> change -segment DEFAULT -file=data.dat
   GDE> exit

Create the database file (data.dat).

.. code-block:: bash

   $ mupip create

Start the GT.CM server.

Note that the global directory created on the remote node in this example is only used by mupip create, and never used by either the client or the server.

**On NODE1**:

On NODE1, invoke YottaDB and perform the following operations:

.. code-block:: bash

   $export ydb_cm_NODE2=6789
   $ydb
   YDB> s ^x=1
   YDB> k ^x
   YDB> s ^y=10
   YDB> h

All these updates should be reported in the NODE2:/testarea/yottadb/database/data.dat file.


