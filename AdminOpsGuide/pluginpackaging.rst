
===========================================
Appendix G: Plugin Packaging
===========================================

A plugin is software that extends the functionality of YottaDB for all users on a system and is installed within the same directory structure as YottaDB. This is the standard structure of a YottaDB installation:

.. parsed-literal::
   $ sudo tree -d /usr/local/lib/yottadb/r126/
   /usr/local/lib/yottadb/r126/
   ├── gtmsecshrdir
   ├── plugin
   │   ├── gtmcrypt
   │   ├── o
   │   │   └── utf8
   │   └── r
   └── utf8
       ├── gtmsecshrdir
       └── plugin -> ../plugin

   9 directories
   $ 

As M does not provide information hiding except for global variables (through global directories), plugins use namespacing to avoid colliding with applications and with one another.

* Each plugin has a case-insensitive unique name even if there is a preferred capitalization, e.g., YottaDBGUI, Octo. As repositories on GitLab have YDB prefixes, e.g., YDBOcto for Octo, plugin names do not start with YDB or YottaDB.

* Except where explicitly required, names use capitalization that would be comfortable for the typical user, or at least for the expected typical code maintainer.

* The preferred package name used to expose a YottaDB API is yottadb, or starts with yottadb.

* Function & method names for languages that do not use package names or other techniques for disambiguation start with ydb, using CamelCase or hunGarian depending on the common practice in the language. Function names for C start with ydb\_.

* Environment variables start with ydb_<pluginname>_, all lower case, e.g., :code:`ydb_octo_gbldir`.

* Local variable names have a %YDB prefix, e.g., all GDE GUI local variables start with %YDBGDEGUI (or %ydbgdegui).

* Global variables can be either those associated with the plugin and its installation (e.g., online help content) or those associated with an application environment or instance (such as cross references and statistics). This distinction between association with the plugin / installation and the application environment / instance also applies to routines, call-in tables and call-out tables. The former should be located based on $ydb_dist and the latter based on :code:`$zroutines` and :code:`$zgbldir`.

  * Global variables associated with a plugin and its installation are in read-only database files using the MM access method (same configuration as YottaDB online help content), accessed using global directories with names of the form <pluginname><gldname>.gld (gldname required only where a package has multiple global directories) and database files of the form <pluginname><dbfilename>.dat (dbfilename required only when a plugin has multiple database files). Global directories and database files reside in :code:`$ydb_dist/plugin/`. There is no restriction on global variable names.

  * Global variables associated with an application environment or instance should not be required to use a separate global directory (e.g., Transactional Consistency is maintained over a replication stream only when references and their cross references use the same global directory), but may allow an application to optionally specify one, e.g., :code:`$ydb_octo_gbldir` defaulting to :code:`$zgbldir` when the environment variable is unspecified. These global variables should start with ^%ydb<pluginname> (lower case is required, as ^%Y* global variables are specially mapped in global directories for statistics databases.

  * M routines associated with a plugin or its installation have names of the form <pluginname><routinename>.m and are placed in :code:`$ydb_dist/plugin/r/`. A prefix of _YDB is not required, but may be used if it simplifies the plugin or is otherwise helpful. The corresponding object code is in shared library files with names <pluginname><sharedlibname>.so (sharedlibname required only if a plugin requires multiple shared libraries), with M mode shared libraries in :code:`$ydb_dist/plugin/o/`. If the underlying YottaDB is installed with UTF-8 mode support at the time a plugin is installed, UTF-8 mode shared libraries are installed in :code:`$ydb_dist/plugin/o/utf8/`.

  * M routines associated with an application environment or instance (e.g., dynamically generated M routines for SQL queries) are named _YDB<pluginname><routinename>.m where one routinename is unlikely to collide with another (e.g., created from a hash of the canonical form of the SQL statement). M routines are placed in the first M routine source directory of :code:`$zroutines`. (To find the first source directory of :code:`$zroutines`, discard any leading space separated pieces that end in .so. Then take the shorter of (a) the the first space separated piece or (b) the first close parenthesis separated piece. From that, take the last open parenthesis separated piece. Discard any trailing asterisk (:code:`*`)). There is no need to generate object code, which will be dynamically generated on first use.

  * Call-in tables associated with a plugin or its installation have names of the form <pluginname><callintabname>.ci (callintabname required only if a plugin requires multiple call-in tables) and are placed in :code:`$ydb_dist/plugin/`. A prefix of _YDB is not required, but may be used if it simplifies the plugin or is otherwise helpful. Since application code may already have a call-in table referenced by :code:`$ydb_ci`, plugin code should use `ydb_ci_tab_get() and ydb_ci_tab_set() <https://gitlab.com/YottaDB/DB/YDB/issues/370>`_ to explicitly set call-in tables before making a call-in, and restore upon return.

  * Call-in tables associated with an application environment or instance are named YDB<pluginname><callintabname>.ci (where callintabname is something meaningful to the purpose of the call-in table, such as the hash of the canonical form of an SQL statement). They are located in the same directory as the M routines they access (i.e., derived from :code:`$zroutines`), and as in the previous item, plugins use :code:`ydb_ci_tab_get()` and :code:`ydb_ci_tab_set()` to access them.

  * Call-out tables associated with a plugin or its installation have names of the form <pluginname>><callouttabname>.xc (callouttabname required only if a plugin requires multiple call-out tables). Shared libraries should have names of the form <pluginname><sharedlibname>.so (sharedlibname required only if a plugin requires multiple shared libraries). Both call-out tables and shared libraries are placed in :code:`$ydb_dist/plugin/`. A prefix of _YDB is not required, but may be used if it simplifies the plugin or is otherwise helpful.

  * Call-out tables associated with an application environment or instance have names of the form YDB<pluginname><callouttabname>.so (callouttabname required only if a plugin requires multiple call-out tables).  C code generated for an application environment or instance should be named ydb<pluginname><routinename>.c. Shared libraries should have names of the form <pluginname><sharedlibname>.so (sharedlibname required only if a plugin requires multiple shared libraries). Call-out tables, C code, and shared libraries are placed in the first routines directory.

  * Top level executables associated with a plugin or installation, e.g., octo are placed in :code:`$ydb_dist/plugin/bin`. In the event the top level is a shell script, the shell should have the package name, and call a binary file with a .bin suffix. e.g., an octo shell script may invoke binary executable :code:`$ydb_dist/plugin/bin/octo.bin`.

Exceptions can be made for use of pre-existing or external software, e.g., the `M Web Server <https://github.com/shabiel/M-Web-Server/tree/master/src>`_. In this case, an explicit decision must be made whether to use it as-is vs. forking it, renaming routines & variable names, and then using the fork. Factors to consider in this decision include:

* The rate of change of the external upstream project. A higher rate of change would suggest use as is. A low rate of change, or abandoned code, suggests forking and rework.
* Whether the names are likely to collide with those of applications if and when they are used together. For example, an M web server used to for the GDE GUI is unlikely to be used with an application and is therefore less likely to have colliding names, unlike, for example, routines for parsing XML/JSON which may be used by many applications.
* The ease of modifying a fork to make its names compliant and the risk of introducing new defects.

