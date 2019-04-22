
.. index::
    About this Manual

========================
 About This Manual
========================

.. contents:: Table of Contents

YottaDB is a high-end database application development platform offered by YottaDB LLC. YottaDB provides an M (also known as MUMPS) language environment that largely complies with ISO/IEC 11756:1999. YottaDB's compiler for the standard M scripting language implements full support for ACID (Atomic, Consistent, Isolated, Durable) transactions, using optimistic concurrency control and software transactional memory (STM) that resolves the common mismatch between databases and programming languages. The YottaDB data model is a hierarchical associative memory (that is, multi-dimensional array) that imposes no restrictions on the data types of the indices and the content - the application logic can impose any schema, dictionary or data organization suited to its problem domain.

YottaDB's unique ability to create and deploy logical multi-site configurations of applications provides unrivaled continuity of business in the face of not just unplanned events, but also planned events, including planned events that include changes to application logic and schema.

You can install and manage YottaDB using the utilities described in this manual and standard operating system tools. The first three chapters provide an overview of YottaDB, installation procedures, and the YottaDB system environment. The remaining chapters describe YottaDB operational management.

-----------------
Intended Audience
-----------------

This manual is intended for users who install YottaDB and manage the YottaDB user environment. The presentation of information assumes a working knowledge of UNIX, but no prior knowledge of YottaDB.

----------------------
Purpose of this Manual
----------------------

This Administration and Operations Guide explains how to install and manage YottaDB.

-----------------------
How to Use This Manual
-----------------------

First, read `Chapter 1: “About YottaDB” <https://docs.yottadb.com/AdminOpsGuide/about.html>`_ for an overview of YottaDB system management. Then, proceed to the chapter that discusses the area of interest.

The presentation of information in each chapter is designed to be useful for even a first-time user. Each chapter starts with an overview of the topic at hand and then moves on to related YottaDB utility program commands and qualifiers. This list is organized alphabetically by command and then by the qualifiers for each command. Then, the chapter provides recommendations from YottaDB to implement and operate important aspects of the topic. It ends with an exercise that reinforces the concepts introduced in the chapter.

We recommend that users read the chapters in a top-down manner. After becoming familiar with YottaDB, use the "Commands and Qualifiers" section of each chapter as a reference manual.

--------
Overview
--------

This manual contains twelve chapters and an Appendix. Here is a brief overview of each chapter:

`Chapter 1: “About YottaDB” <https://docs.yottadb.com/AdminOpsGuide/about.html>`_ introduces YottaDB administration and operations.

`Chapter 2: “Installing YottaDB” <https://docs.yottadb.com/AdminOpsGuide/installydb.html>`_ provides procedures for installing YottaDB.

`Chapter 3: “Basic Operations” <https://docs.yottadb.com/AdminOpsGuide/basicops.html>`_ describes operations required to start YottaDB and keep it running.

`Chapter 4: “Global Directory Editor” <https://docs.yottadb.com/AdminOpsGuide/gde.html>`_ describes global directories, which control placement and access of global variables, and explains how to use the Global Directory Editor (GDE) to create and maintain global directories.

`Chapter 5: “General Database Management” <https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html>`_ describes how to use a YottaDB utility called MUPIP to perform database and non-database operations.

`Chapter 6: “YottaDB Journaling” <https://docs.yottadb.com/AdminOpsGuide/ydbjournal.html>`_ describes the journaling capabilities of YottaDB.

`Chapter 7: “Database Replication” <https://docs.yottadb.com/AdminOpsGuide/dbrepl.html>`_ describes how to implement continuous application availability using multiple systems.

`Chapter 8: “M Lock Utility (LKE)”  <https://docs.yottadb.com/AdminOpsGuide/mlocks.html>`_ describes how to use a utility called the M Lock Utility (LKE) to examine and adjust M locks.

`Chapter 9: “YottaDB Database Structure(GDS)” <https://docs.yottadb.com/AdminOpsGuide/gds.html>`_ provides an overview of YottaDB Database Structure (GDS).

`Chapter 10: “Database Structure Editor” <https://docs.yottadb.com/AdminOpsGuide/dse.html>`_ describes how to use the Database Structure Editor (DSE) to examine and modify sections of the database, should that ever be necessary.

`Chapter 11: “Maintaining Database Integrity” <https://docs.yottadb.com/AdminOpsGuide/integrity.html>`_ describes procedures for verifying and maintaining the integrity of GDS databases.

`Chapter 12: “Database Encryption” <https://docs.yottadb.com/AdminOpsGuide/encryption.html>`_ describes procedures for encrypting data in the database and journal files.

-------------------------------
Conventions Used in this Manual
-------------------------------

Use YottaDB with any UNIX shell as long as environment variables and scripts are consistently created for that shell. In this manual, UNIX examples are validated on Ubuntu Linux (Ubuntu Linux uses dash for /bin/sh). Examples in later chapters assume that an environment has been set up as described in the chapters `Chapter 2: “Installing YottaDB” <https://docs.yottadb.com/AdminOpsGuide/installydb.html>`_ and `Chapter 3: “Basic Operations” <https://docs.yottadb.com/AdminOpsGuide/basicops.html>`_.

We have made a conscientious effort to present intuitive examples and related error messages that appear if a user tries those examples. However, due to environment and shell differences, you may occasionally obtain different results (although the differences should be relatively minor). Therefore, it is suggested that you try the examples in a database environment that does not contain any valued information.

With the M examples, an effort was made to construct examples where command lines did not wrap, in many cases using the argumentless DO.

The examples make frequent use of literals in an attempt to focus attention on particular points. In normal usage, arguments are far more frequently variables.

