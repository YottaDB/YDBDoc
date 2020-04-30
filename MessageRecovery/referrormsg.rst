.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.     #
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This source code contains the intellectual property     #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. toctree::
   :glob:

.. index::
   Reference Implementation Error Messages

========================================
Reference Implementation Error Messages
========================================

--------------------------------
Cannot find DB keys file <path>
--------------------------------

Plugin error: The plugin cannot find the master key file.

Action: Set the $gtm_dbkeys environment variable to point to the correct master key file.

------------------------------------------
Cannot find Yottadb executable in <path>
------------------------------------------

Plugin error: The plugin cannot find the yottadb executable.

Action: Set the $ydb_dist environment variable to the directory containing the yottadb executable. Verify proper permissions for directory path and file.

------------------------------------
Cannot open DB keys file - <path>
------------------------------------

Plugin error: The plugin cannot open the master key file.

Action: Verify that the master key file exists and there are appropriate authorizations on the directory path and master key file.

------------------------------------------
DB keys file of unknown file type : <path>
------------------------------------------

Plugin error: The plugin reports that the master key file is not the proper type file.

Action: Point the gtm_dbkeys environment variable to an appropriately formatted master key file.

--------------------------------------------------------------
Database file <path> missing in DB keys file or does not exist
--------------------------------------------------------------

Plugin error: The plugin reports that the master key file does not contain a valid entry pointing to the database file.

Action: Create an entry in the master key file for the specified database file, verify that the database file exists and that appropriate authorizations exist on the directory and database file name.

------------------------------------------------------
Database file <path> not found
------------------------------------------------------

Plugin error: The plugin is unable to find the specified database file.

Action: Verify that the database file exists, the corresponding entry in the master key file points to the database file, and appropriate authorizations exist in the directory path and the database file.

-------------------------------------
Encryption handle corrupted
-------------------------------------

Plugin error: The plugin detected an internal error.

Action: This error indicates that there is a communication error between YottaDB and the gtmcrypt plug-in. Replace the process with an undamaged one. Report the entire incident context to your YottaDB support channel.

-------------------------------------
Encryption key file <path> not found
-------------------------------------

Plugin error: The plugin was not able to find the key file on the specified path.

Action: Verify that the master key file entry for this key file points to the correct path. Verify that the key file itself exists. Verify proper authorizations on directory path and file.

--------------------------------------------
Encryption library has not been initialized
--------------------------------------------

Plugin error: A gtmcrypt function was called before gtmcrypt_init().

Action: Call gtmcrypt_init() before calling any other encryption function.

---------------------------------------------------
Environment variable <environment_variable> not set
---------------------------------------------------

Plugin error: An environment variable needed by the plugin was not set.

Action: Set the environment variable <environment_variable> to an appropriate value.

-----------------------------------------------------
Environment variable gtm_dbkeys set to empty string
-----------------------------------------------------

Plugin error: The $gtm_dbkeys environment variable was set to the empty string.

Recovery Action: Set $gtm_dbkeys to point to the master key file.

-------------------------------------------------------------------------
Environment variable gtm_dbkeys undefined. Cannot find <path>/.gtm_dbkeys
-------------------------------------------------------------------------

Plugin error: The plugin was unable to locate the master key file.

Action: Place the master key file in the users home directory or point the gtm_dbkeys environment variable to the master key file.

--------------------------------------------------------------------------------------------------
Environment variable ydb_passwd set to empty string. Password prompting not allowed for utilities
--------------------------------------------------------------------------------------------------

Plugin error: The plugin detected that it needed the obfuscated password but the $ydb_passwd environment variable was set to the empty string.

Action: Use maskpass to set $ydb_passwd to the obfuscated password prior to invoking MUPIP or DSE, or wrap the utility invocation with a M process which will prompt for the password and set the obfuscated password.

------------------------------------------------------------------------------------
Error initializing GpgME: <reason_for_error>/<specific_Gpg_ME_error>
------------------------------------------------------------------------------------

Plugin error: libgpgme reported an error to the plugin.

Action: Consult GpgME documentation for the specific error message.

-----------------------------------------------------------------------------------------------------------
Error parsing database key file. At line <line_number>: No matching 'dat' entry found in <contents_of_line>
-----------------------------------------------------------------------------------------------------------

Plugin error: The plugin was unable to find a matching "dat" entry for a "key" entry in the master key file.

Action: Verify that each "key" entry has a corresponding "dat" entry.

---------------------------------------------------------------------------------------------------------
Error parsing database key file. At line <line_number>: <line_contents> does not start with 'dat' / 'key'
---------------------------------------------------------------------------------------------------------

Plugin error: The plugin detected that the master key file was not properly formatted.

Action: Verify that the entries in the master key file start with "dat" or "key".

------------------------------------------------------------------------------------------------------------
Error parsing database key file. At line <line_number>: No matching 'key' entry found in <contents_of_line>
------------------------------------------------------------------------------------------------------------

Plugin error: The plugin was not able to find a "key" entry for a "dat" entry.

Action: Verify that the database file exists, that the corresponding entry in the master key file points to the database file, that appropriate authorizations exist on the directory path and the database file, and that each "dat" entry has a corresponding "key" entry.

--------------------------------------------------------
Incorrect password
--------------------------------------------------------

Plugin error: The plugin detected that the correct private key password was not supplied.

Action: Provide the correct password.

-------------------------------------------------------------------------------
libgcrypt version mismatch. Expected <expected_version>, found <found_version>
-------------------------------------------------------------------------------

Plugin error: The plugin could not locate an appropriate libgcrypt library version.

Action: Verify that the <expected_version> is installed and in the library search path.

--------------------------------------------------------------
Matching encryption key <hash> not found in database key file
--------------------------------------------------------------

Plugin error: The plugin was not able to find a needed database file key.

Action: Add an entry for this encryption key. If needed, use DSE DUMP -FILE -ALL on all of the database files to find the database file that matches the <hash>. With extracts and backups, multiple database files may have contributed encrypted records.

---------------------------------------------------
No entries found in DB keys file
---------------------------------------------------

Plugin error: The plugin was unable to find any entries in the master key file.

Action: Add entries to the master key file.

----------------------------------------------
Symmetric key <path> found to be empty
----------------------------------------------

Plugin error: The plugin was unable to find a valid encrypted key in the specified file.

Action: Create a valid key file.
