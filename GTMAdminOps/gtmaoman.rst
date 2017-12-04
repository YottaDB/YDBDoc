.. header::
   YottaDB - GT.M Admin and Ops - About this manual

.. footer::
   Page ###Page### of ###Total###

.. index::
   GT.M : About this Manual

========================
GT.M : About This Manual
========================

.. contents:: Table of Contents

GT.M is a high-end database application development platform offered by Fidelity National Information Services (FIS). GT.M provides an M (also known as MUMPS) language environment that largely complies with ISO/IEC 11756:1999. GT.M's compiler for the standard M scripting language implements full support for ACID (Atomic, Consistent, Isolated, Durable) transactions, using optimistic concurrency control and software transactional memory (STM) that resolves the common mismatch between databases and programming languages. The GT.M data model is a hierarchical associative memory (that is, multi-dimensional array) that imposes no restrictions on the data types of the indexes and the content - the application logic can impose any schema, dictionary or data organization suited to its problem domain.

GT.M's unique ability to create and deploy logical multi-site configurations of applications provides unrivaled continuity of business in the face of not just unplanned events, but also planned events, including planned events that include changes to application logic and schema.

You can install and manage GT.M using the utilities described in this manual and standard operating system tools. The first three chapters provide an overview of GT.M, installation procedures, and GT.M system environment. The remaining chapters describe GT.M operational management.

-----------------
Intended Audience
-----------------

This manual is intended for users who install GT.M and manage the GT.M user environment. The presentation of information assumes a working knowledge of UNIX, but no prior knowledge of GT.M.

----------------------
Purpose of this Manual
----------------------

This GT.M Administration and Operations Guide explains how to install and manage GT.M.

-----------------------
How to Use This Manual
-----------------------

First, read Chapter 1: “About GT.M” for an overview of GT.M system management. Then, proceed to the chapter that discusses the area of interest.

The presentation of information in each chapter is designed to be useful for even a first-time user. Each chapter starts with an overview of the topic at hand and then moves on to related GT.M utility program commands and qualifiers.This list is organized alphabetically by command and then by the qualifiers for each command. Then, the chapter provides recommendations from FIS to implement and operate important aspects of the topic. It ends with an exercise that reinforces the concepts introduced in the chapter.

FIS recommends users read the chapters in a top-down manner. After becoming familiar with GT.M, use the "Commands and Qualifiers" section of each chapter as a reference manual.

The left frame in the HTML version of the manuals includes a Google custom search engine for searching across the manual. It requires an Internet connection and a Javascript-enabled browser.

--------
Overview
--------

This manual contains twelve chapters and an Appendix. Here is a brief overview of each chapter:

Chapter 1: “About GT.M” introduces GT.M administration and operations.

Chapter 2: “Installing GT.M” provides procedures for installing GT.M.

Chapter 3: “Basic Operations” describes operations required to start GT.M and keep it running.

Chapter 4: “Global Directory Editor” describes global directories, which control placement and access of global variables, and explains how to use the Global Directory Editor (GDE) to create and maintain global directories.

Chapter 5: “General Database Management” describes how to use a GT.M utility called MUPIP to perform database and non-database operations.

Chapter 6: “GT.M Journaling” describes the journaling capabilities of GT.M.

Chapter 7: “Database Replication” describes how to implement continuous application availability using multiple systems.

Chapter 8: “M Lock Utility (LKE)” describes how to use a GT.M utility called M Lock Utility (LKE) to examine and adjust M locks.

Chapter 9: “GT.M Database Structure(GDS)” provides an overview of GT.M Database Structure (GDS).

Chapter 10: “Database Structure Editor” describes how to use the Database Structure Editor (DSE) to examine and modify sections of the database, should that ever be necessary.

Chapter 11: “Maintaining Database Integrity” describes procedures for verifying and maintaining the integrity of GDS databases.

Chapter 12: “Database Encryption” describes procedures for encrypting data in the database and journal files.

Appendix A: “GT.M's IPC Resource Usage” describes how GT.M processes use UNIX Interprocess Communication (IPC).

Appendix C: “Compiling ICU on GT.M supported platforms” contains sample ICU installation instructions. Although GT.M uses ICU to enable the support for Unicode™, ICU is not FIS software and FIS does not support ICU. The sample instructions for installing and configuring ICU are merely provided as a convenience to you.

"Appendix C" describes security considerations for deploying applications on GT.M.

-------------------------------
Conventions Used in this Manual
-------------------------------

Use GT.M with any UNIX shell as long as environment variables and scripts are consistently created for that shell. In this manual, UNIX examples are validated on Ubuntu Linux (Ubuntu Linux uses dash for /bin/sh).Examples in later chapters assume that an environment has been set up as described in the chapters Chapter 2: “Installing GT.M” and Chapter 3: “Basic Operations”.

FIS made a conscientious effort to present intuitive examples and related error messages that appear if a user tries those examples. However, due to environment and shell differences, you may occasionally obtain different results (although the differences should be relatively minor). Therefore, FIS suggests that you try the examples in a database environment that does not contain any valued information.

In M examples, an effort was made to construct examples where command lines did not wrap, in many cases using the argumentless DO.

The examples make frequent use of literals in an attempt to focus attention on particular points. In normal usage arguments are far more frequently variables.

