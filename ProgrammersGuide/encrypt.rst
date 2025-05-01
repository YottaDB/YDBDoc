.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2018-2025 YottaDB LLC and/or its subsidiaries.#
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

.. index::
   Database Encryption Extensions to the YottaDB External Interface

====================================================================
15. Database Encryption Extensions to the YottaDB External Interface
====================================================================

To support Database Encryption, YottaDB provides a reference implementation which resides in :code:`$ydb_dist/plugin/gtmcrypt`.

The reference implementation includes:

* A :code:`$ydb_dist/plugin/gtmcrypt` sub-directory with all source files and scripts. The scripts include those needed to build/install :code:`libgtmcrypt.so` and "helper" scripts, for example, :code:`add_db_key.sh` (see below).
* The plugin interface that YottaDB expects is defined in :code:`gtmcrypt_interface.h`. **Never** modify this file - it defines the interface that the plugin must provide.
* :code:`$ydb_dist/plugin/libgtmcrypt.so` is the shared library containing the executables which is dynamically linked by YottaDB and which in turn calls the encryption packages. If the :code:`$ydb_dist/utf8` directory exists, then it should contain a symbolic link to ../plugin.
* Source code is provided in the file :code:`$ydb_dist/plugin/gtmcrypt/source.tar` which includes build.sh and install.sh scripts to respectively compile and install libgtmcrypt.so from the source code.

To support the implementation of a reference implementation, YottaDB provides additional C structure types (in the libyottadb.h file):

* :code:`gtmcrypt_key_t` - a datatype that is a handle to a key. The YottaDB database engine itself does not manipulate keys. The plug-in keeps the keys, and provides handles to keys that the YottaDB database engine uses to refer to keys.
* :code:`xc_fileid_ptr_t` - a pointer to a structure maintained by YottaDB to uniquely identify a file. Note that a file may have multiple names - not only as a consequence of absolute and relative path names, but also because of symbolic links and also because a file system can be mounted at more than one place in the file name hierarchy. YottaDB needs to be able to uniquely identify files.

Although not required to be used by a customized plugin implementation, YottaDB provides (and the reference implementation uses) the following functions for uniquely identifying files:

* :code:`xc_status_t ydb_filename_to_id(xc_string_t \*filename, xc_fileid_ptr_t \*fileid)` - function that takes a file name and provides the file id structure for that file.
* :code:`xc_status_t ydb_is_file_identical(xc_fileid_ptr_t fileid1, xc_fileid_ptr_t fileid2)` - function that determines whether two file ids map to the same file.
* :code:`ydb_xcfileid_free(xc_fileid_ptr_t fileid)` - function to release a file id structure.

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

A YottaDB database consists of multiple database files, each of which has its own encryption key, although you can use the same key for multiple files. Thus, the gtmcrypt* functions are capable of managing multiple keys for multiple database files. Prototypes for these functions are in :code:`gtmcrypt_interface.h`.

The core plugin interface functions, all of which return a value of type ydb_status_t are:

* :code:`gtmcrypt_init()` performs initialization. If the environment variable :code:`$ydb_passwd` exists and has an empty string value, YottaDB calls gtmcrypt_init() before the first M program is loaded; otherwise it calls gtmcrypt_init() when it attempts the first operation on an encrypted database file.
* Generally, :code:`gtmcrypt_getkey_by_hash()` or, for MUPIP CREATE, :code:`gtmcrypt_getkey_by_name()` perform key acquisition, and place the keys where :code:`gtmcrypt_decode()` and :code:`gtmcrypt_encode()` can find them when they are called.
* Whenever YottaDB needs to decode a block of bytes, it calls :code:`gtmcrypt_decode()` to decode the encrypted data. At the level at which YottaDB database encryption operates, it does not matter what the data is - numeric data, string data whether in M or UTF-8 mode and whether or not modified by a collation algorithm. Encryption and decryption simply operate on a series of bytes.
* Whenever YottaDB needs to encode a block of bytes, it calls :code:`gtmcrypt_encode()` to encode the data.
* If encryption has been used (if :code:`gtmcrypt_init()` was previously called and returned success), YottaDB calls :code:`gtmcrypt_close()` at process exit and before generating a core file. gtmcrypt_close() must erase keys in memory to ensure that no cleartext keys are visible in the core file.

More detailed descriptions follow.

* :code:`gtmcrypt_key_t \*gtmcrypt_getkey_by_name(ydb_string_t \*filename)` - MUPIP CREATE uses this function to get the key for a database file. This function searches for the given filename in the memory key ring and returns a handle to its symmetric cipher key. If there is more than one entry for the given filename , the reference implementation returns the entry matching the last occurrence of that filename in the master key file.
* :code:`ydb_status_t gtmcrypt_hash_gen(gtmcrypt_key_t \*key, ydb_string_t \*hash)` - MUPIP CREATE uses this function to generate a hash from the key then copies that hash into the database file header. The first parameter is a handle to the key and the second parameter points to 256 byte buffer. In the event the hash algorithm used provides hashes smaller than 256 bytes, gtmcrypt_hash_gen() must fill any unused space in the 256 byte buffer with zeros.
* :code:`gtmcrypt_key_t \*gtmcrypt_getkey_by_hash(ydb_string_t \*hash)` - YottaDB uses this function at database file open time to obtain the correct key using its hash from the database file header. This function searches for the given hash in the memory key ring and returns a handle to the matching symmetric cipher key. MUPIP LOAD, MUPIP RESTORE, MUPIP EXTRACT, MUPIP JOURNAL and MUPIP BACKUP -BYTESTREAM all use this to find keys corresponding to the current or prior databases from which the files they use for input were derived.
* :code:`ydb_status_t gtmcrypt_encode(gtmcrypt_key_t \*key, ydb_string_t \*inbuf, ydb_string_t \*outbuf)` and :code:`ydb_status_t gtmcrypt_decode(gtmcrypt_key_t \*key, ydb_string_t \*inbuf, ydb_string_t \*outbuf)`- YottaDB uses these functions to encode and decode data. The first parameter is a handle to the symmetric cipher key, the second is a pointer to the block of data to encode or decode, and the third is a pointer to the resulting block of encoded or decoded data. Using the appropriate key (same key for a symmetric cipher), gtmcrypt_decode() must be able to decode any data buffer encoded by gtmcrypt_encode(), otherwise the encrypted data is rendered unrecoverable. As discussed earlier, YottaDB requires the encrypted and cleartext versions of a string to have the same length.
* :code:`char \*gtmcrypt_strerror()` - YottaDB uses this function to retrieve addtional error context from the plug-in after the plug-in returns an error status. This function returns a pointer to additional text related to the last error that occurred. YottaDB displays this text as part of an error report. In a case where an error has no additional context or description, this function returns a null string.

The complete source code for reference implementations of these functions is provided, licensed under the same terms as YottaDB. You are at liberty to modify them to suit your specific YottaDB database encryption needs.

For more information and examples, refer to `Database Encryption <../AdminOpsGuide/encryption.html>`_ in the Administration and Operations Guide.

YottaDB provides the following functions to defer interrupts, invoke the SSL/TLS function and enable interrupts:

* :code:`const char *gtm_tls_get_error(void)` - Returns the most recent error (null-terminated) related to the workings of the SSL/TLS reference implementation.
* :code:`int gtm_tls_errno(void)` - If the most recent invocation of the SSL/TLS reference implementation resulted in a system call error, gtm_tls_errno() returns the value of errno. Otherwise, -1 is returned in which case gtm_tls_get_error() provides more information.
* :code:`gtm_tls_ctx_t *gtm_tls_init(int version, int flags)` - Initializes the SSL/TLS context for a process. Typically invoked only once (unless the previous attempt failed). Attributes necessary to initialize the SSL/TLS context are obtained from the configuration file pointed to by $ydb_crypt_config.
* :code:`int gtm_tls_store_passwd(gtm_tls_ctx_t *tls_ctx, const char *tlsid, const char *obs_passwd)` - Stores a M program provided password for later use.
* :code:`int gtm_tls_add_config(gtm_tls_ctx_t *tls_ctx, const char *idstr, const char *configstr)` - Provides additional information to merge with config file.
* :code:`void gtm_tls_prefetch_passwd(gtm_tls_ctx_t *tls_ctx, char *env_name)` - Prefetches the password corresponding to a private key.
* :code:`gtm_tls_socket_t *gtm_tls_socket(gtm_tls_ctx_t *ctx, gtm_tls_socket_t *prev_socket, int sockfd, char *id, int flags)` - Converts a Unix TCP/IP socket into a SSL/TLS aware socket.
* :code:`int gtm_tls_connect(gtm_tls_socket_t *socket)` - Connects using SSL/TLS aware socket. Assumes the other transport endpoint understands SSL/TLS.
* :code:`int gtm_tls_accept(gtm_tls_socket_t *socket)` - Accepts an incoming connection using SSL/TLS aware socket. Assumes the other transport endpoint understands SSL/TLS.
* :code:`int gtm_tls_renegotiate(gtm_tls_socket_t *socket)` - Renegotiates an active SSL/TLS connection. This function does the renegotiation in a blocking fashion and more importantly handles EINTR internally by retrying the renegotiation.
* :code:`int gtm_tls_renegotiate_options(gtm_tls_socket_t *socket, int msec_timeout, char *idstr, char *configstr, int tlsid_present)` - Process configuration file options for WRITE /TLS("renegotiate") and then calls gtm_tls_renegotiate.
* :code:`int gtm_tls_get_conn_info(gtm_tls_socket_t *socket, gtm_tls_conn_info *conn_info)` - Obtains additional SSL/TLS related information on the peer. This function is typically invoked to log information for diagnostic purposes.
* :code:`int gtm_tls_send(gtm_tls_socket_t *socket, char *buf, int send_len)` - Transmits message securely to the transport endpoint. This function should be invoked ONLY after successful invocations of either gtm_tls_connect() or gtm_tls_accept().
* :code:`int gtm_tls_recv(gtm_tls_socket_t *socket, char *buf, int recv_len)` - Receives message securely from the transport endpoint. This function should be invoked ONLY after successful invocations of either gtm_tls_connect() or gtm_tls_accept().
* :code:`int gtm_tls_cachedbytes(gtm_tls_socket_t *socket)` - Returns the number of bytes cached in the SSL/TLS layer and is ready for immediate retrieval with the gtm_tls_recv().
* :code:`void gtm_tls_socket_close(gtm_tls_socket_t *socket)` - Close the SSL/TLS socket connection.
* :code:`void gtm_tls_session_close(gtm_tls_socket_t **socket)` - Closes an active SSL/TLS session. This frees up the session and thus makes the session not resuable for a future connection. Any subsequent connection will create a new session.
* :code:`void gtm_tls_fini(gtm_tls_ctx_t **ctx)` - Frees up any memory allocated by the SSL/TLS context. This function should typically be invoked at process exit.

For detailed explanation on the above functions refer to `ydb_tls_interface.h <https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydb_tls_interface.h>`_ file.

.. raw:: html

    <img referrerpolicy="no-referrer-when-downgrade" src="https://download.yottadb.com/MProgGuide.png" />
