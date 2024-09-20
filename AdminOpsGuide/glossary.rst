.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2017-2024 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. # Portions Copyright (c) Fidelity National                    #
.. # Information Services, Inc. and/or its subsidiaries.         #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. index:: Glossary

==================
Glossary
==================

--
I
--

**In-flight transactions** : In-flight transactions are those transactions that have been committed (or hardened, or made Durable) on an originating instance (or a propagating instance) but not yet transmitted to a replicating instance.

--
O
--

**Originating Instance** : An originating instance is an instance that processes business logic and generates logical updates to the database.

--
R
--

**Replicating Instance** : A replicating instance is an instance that does not execute business logic or generate database updates. It receives logical database updates from an originating instance or another replicating instance and applies them to its database.

**Receiving Instance** : Within the context of a replication connection between two instances, a replicating instance that receives updates from a source instance is referred to as receiving instance or receiver side. For example, in an B←A→C replication configuration, both B and C can be referred to as a receiving instance.

--
S
--

**Source Instance** : Within the context of a replication connection between two instances, an originating instance is referred to as source instance or source side. For example, in an B←A→C replication configuration, A is the source instance for B and C.

**Supplementary Instance** : A supplementary instance is an instance that processes business logic, generates logical updates to the database, and at the same time receives updates from another instance.

.. raw:: html

    <img referrerpolicy="no-referrer-when-downgrade" src="https://download.yottadb.com/AdminOpsGuide.png" />
