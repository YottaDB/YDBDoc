.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2018-2022 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. index::
   Database Encryption Extensions to the YottaDB External Interface

====================================================================
15. Database Encryption Extensions to the YottaDB External Interface
====================================================================

To support Database Encryption, YottaDB provides a reference implementation which resides in $ydb_dist/plugin/gtmcrypt.

The reference implementation includes:

* A $ydb_dist/plugin/gtmcrypt sub-directory with all source files and scripts. The scripts include those needed to build/install libgtmcrypt.so and "helper" scripts, for example, add_db_key.sh (see below).
* The plugin interface that YottaDB expects is defined in gtmcrypt_interface.h. Never modify this file - it defines the interface that the plugin must provide.
* $ydb_dist/plugin/libgtmcrypt.so is the shared library containing the executables which is dynamically linked by YottaDB and which in turn calls the encryption packages. If the $ydb_dist/utf8 directory exists, then it should contain a symbolic link to ../plugin.
* Source code is provided in the file $ydb_dist/plugin/gtmcrypt/source.tar which includes build.sh and install.sh scripts to respectively compile and install libgtmcrypt.so from the source code.

To support the implementation of a reference implementation, YottaDB provides additional C structure types (in the libyottadb.h file):

* gtmcrypt_key_t - a datatype that is a handle to a key. The YottaDB database engine itself does not manipulate keys. The plug-in keeps the keys, and provides handles to keys that the YottaDB database engine uses to refer to keys.
* xc_fileid_ptr_t - a pointer to a structure maintained by YottaDB to uniquely identify a file. Note that a file may have multiple names - not only as a consequence of absolute and relative path names, but also because of symbolic links and also because a file system can be mounted at more than one place in the file name hierarchy. YottaDB needs to be able to uniquely identify files.

Although not required to be used by a customized plugin implementation, YottaDB provides (and the reference implementation uses) the following functions for uniquely identifying files:

* xc_status_t ydb_filename_to_id(xc_string_t \*filename, xc_fileid_ptr_t \*fileid) - function that takes a file name and provides the file id structure for that file.
* xc_status_t ydb_is_file_identical(xc_fileid_ptr_t fileid1, xc_fileid_ptr_t fileid2) - function that determines whether two file ids map to the same file.
* ydb_xcfileid_free(xc_fileid_ptr_t fileid) - function to release a file id structure.

M, MUPIP and DSE processes dynamically link to the plugin interface functions that reside in the shared library. The functions serve as software "shims" to interface with an encryption library such as libmcrypt or libgpgme/libgcrypt.

The plugin interface functions are:

* gtmcrypt_init()
* gtmcrypt_getkey_by_name()
* gtmcrypt_getkey_by_hash()
* gtmcrypt_hash_gen()
* gtmcrypt_encode()
* gtmcrypt_decode()
* gtmcrypt_close()
* and gtmcrypt_strerror()

A YottaDB database consists of multiple database files, each of which has its own encryption key, although you can use the same key for multiple files. Thus, the gtmcrypt* functions are capable of managing multiple keys for multiple database files. Prototypes for these functions are in gtmcrypt_interface.h.

The core plugin interface functions, all of which return a value of type ydb_status_t are:

* gtmcrypt_init() performs initialization. If the environment variable $ydb_passwd exists and has an empty string value, YottaDB calls gtmcrypt_init() before the first M program is loaded; otherwise it calls gtmcrypt_init() when it attempts the first operation on an encrypted database file.
* Generally, gtmcrypt_getkey_by_hash or, for MUPIP CREATE, gtmcrypt_getkey_by_name perform key acquisition, and place the keys where gtmcrypt_decode() and gtmcrypt_encode() can find them when they are called.
* Whenever YottaDB needs to decode a block of bytes, it calls gtmcrypt_decode() to decode the encrypted data. At the level at which YottaDB database encryption operates, it does not matter what the data is - numeric data, string data whether in M or UTF-8 mode and whether or not modified by a collation algorithm. Encryption and decryption simply operate on a series of bytes.
* Whenever YottaDB needs to encode a block of bytes, it calls gtmcrypt_encode() to encode the data.
* If encryption has been used (if gtmcrypt_init() was previously called and returned success), YottaDB calls gtmcrypt_close() at process exit and before generating a core file. gtmcrypt_close() must erase keys in memory to ensure that no cleartext keys are visible in the core file.

More detailed descriptions follow.

* gtmcrypt_key_t \*gtmcrypt_getkey_by_name(ydb_string_t \*filename) - MUPIP CREATE uses this function to get the key for a database file. This function searches for the given filename in the memory key ring and returns a handle to its symmetric cipher key. If there is more than one entry for the given filename , the reference implementation returns the entry matching the last occurrence of that filename in the master key file.
* ydb_status_t gtmcrypt_hash_gen(gtmcrypt_key_t \*key, ydb_string_t \*hash) - MUPIP CREATE uses this function to generate a hash from the key then copies that hash into the database file header. The first parameter is a handle to the key and the second parameter points to 256 byte buffer. In the event the hash algorithm used provides hashes smaller than 256 bytes, gtmcrypt_hash_gen() must fill any unused space in the 256 byte buffer with zeros.
* gtmcrypt_key_t \*gtmcrypt_getkey_by_hash(ydb_string_t \*hash) - YottaDB uses this function at database file open time to obtain the correct key using its hash from the database file header. This function searches for the given hash in the memory key ring and returns a handle to the matching symmetric cipher key. MUPIP LOAD, MUPIP RESTORE, MUPIP EXTRACT, MUPIP JOURNAL and MUPIP BACKUP -BYTESTREAM all use this to find keys corresponding to the current or prior databases from which the files they use for input were derived.
* ydb_status_t gtmcrypt_encode(gtmcrypt_key_t \*key, ydb_string_t \*inbuf, ydb_string_t \*outbuf) and ydb_status_t gtmcrypt_decode(gtmcrypt_key_t \*key, ydb_string_t \*inbuf, ydb_string_t \*outbuf)- YottaDB uses these functions to encode and decode data. The first parameter is a handle to the symmetric cipher key, the second is a pointer to the block of data to encode or decode, and the third is a pointer to the resulting block of encoded or decoded data. Using the appropriate key (same key for a symmetric cipher), gtmcrypt_decode() must be able to decode any data buffer encoded by gtmcrypt_encode(), otherwise the encrypted data is rendered unrecoverable. As discussed earlier, YottaDB requires the encrypted and cleartext versions of a string to have the same length.
* char \*gtmcrypt_strerror() - YottaDB uses this function to retrieve addtional error context from the plug-in after the plug-in returns an error status. This function returns a pointer to additional text related to the last error that occurred. YottaDB displays this text as part of an error report. In a case where an error has no additional context or description, this function returns a null string.

The complete source code for reference implementations of these functions is provided, licensed under the same terms as YottaDB. You are at liberty to modify them to suit your specific YottaDB database encryption needs.

For more information and examples, refer to `Database Encryption <../AdminOpsGuide/encryption.html>`_ in the Administration and Operations Guide.
