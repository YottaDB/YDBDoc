
.. index::
   TLS Configuration File

==============================================
Appendix F : Creating a TLS Configuration File
==============================================

.. contents::
   :depth: 2

------------------------------------
Creating a TLS Configuration File
------------------------------------

YottaDB ships with a reference implementation of the encryption plugin, which uses OpenSSL to perform various operations for TLS replication and/or TLS-enabled sockets.

OpenSSL options are controlled by a configuration file in libconfig format. This configuration file is pointed to by the ydb_crypt_config environment variable. The use of the ydb_crypt_config environment variable requires the libconfig library to be installed.

A configuration file is divided into two sections: a database encryption section and a TLS section. The database encryption section contains a list of database files and their corresponding key files. You do not need a database encryption section if you are not using an encrypted database, nor a TLS section if you are not using TLS for replication or sockets. The TLS section contains information needed for OpenSSL, such as the location of root certification authority certificates and leaf-level certificates with their corresponding private keys.

Here is a sample configuration file:

.. parsed-literal::
   /* Database encryption section \*/
    database: {
        keys: (
              {
                 dat: "/tmp/mumps.dat";  /* Encrypted database file. \*/
                 key: "/tmp/mumps.key";  /* Encrypted symmetric key. \*/
              },
              {
                 dat: "/tmp/a.dat";
                 key: "/tmp/a.key";
              },
              ...
        );
    }

    /* TLS section \*/

    tls: {
           verify-depth: 7;
           CAfile: "/home/jdoe/current/tls/certs/CA/ydbCA.crt";
           CApath: "/home/jdoe/current/tls/certs/CA/";
           dh512: "/home/jdoe/current/tls/dh512.pem";
           dh1024: "/home/jdoe/current/tls/dh1024.pem";
           session-timeout: 600;

           /* List of certificate/key pairs specified by identifiers. \*/
           PRODUCTION: {
                         format: "PEM";
                         /* Path to the certificate. \*/
                         cert: "/home/jdoe/current/tls/certs/Malvern.crt";
                         key: "/home/jdoe/current/tls/certs/Malvern.key";
           };

           DEVELOPMENT: {
                          format: "PEM";
                          cert: "/home/jdoe/current/tls/certs/BrynMawr.crt";
                          key: "/home/jdoe/current/tls/certs/BrynMawr.key";
           };
     };


The environment variable ydb_tls_passwd_<tlsid> must specify an obfuscated version of the password for the client's private key. Use the maskpass utility provided with your YottaDB distribution to create an obfuscated password.

The supported OpenSSL options are as follows:

**CAFile**

When used on the tls level (or the tlsid level if not provided at the tls level), it points to a file (in PEM format) describing the trusted CAs. The file can contain several CA certificates identified by sequences of:

.. parsed-literal::
   -----BEGIN CERTIFICATE-----
   ... (CA certificate in base64 encoding) ...
   -----END CERTIFICATE-----

When specified for a server connection either in a tlsid level configuration file section or with a WRITE/TLS command, allows the server to inform the client of acceptable certificate authorities via the OpenSSL function SSL_set_client_CA_list(). The determinant definition for the acceptable list of certificate authorities sent to the client comes in descending order of priority from the one specified by the WRITE /TLS("renegotiate",...) command, the one specified by the CAfile value in the tlsid section used to establish the TLS connection and finally, the one specified at the tls level.

**CApath**

Points to a directory containing CA certificates in PEM format. The files each contain one CA certificate. The files are looked up by the CA subject name hash value, which must hence be available. If more than one certificate with the same name hash value exists, the extension must be different (e.g. 9d66eef0.0, 9d66eef0.1 etc). The directory is typically created by the OpenSSL tool 'c_rehash'.

**cipher-list**

Specifies which cryptographic algorithms to use. The format of this option is described by the OpenSSL ciphers man page. An empty string uses a default value of "ALL:!ADH:!LOW:!EXP:!MD5:@STRENGTH" for replication connections and the OpenSSL default cipher list for socket connections.

**crl**

Points to a file containing a list of revoked certificates. This file is created by the openssl utility.

**dh512 and dh1024**

Specifies that Diffie-Hellman parameters are used for key-exchange. Either none or both have to be specified. If neither is specified, then the data is encrypted with the same keys that are used for authentication.

**format**

Format of the certificate and private key pair. Currently, the YottaDB TLS plug-in only supports PEM format.

**cert**

Path to the certificate.

**key**

Path to the private key. If the private key is protected by a passphrase, an obfuscated version of the password should be specified in the environment variable which takes the form ydb_tls_passwd_<identifier>. Currently, the YottaDB TLS plug-in only supports RSA private keys.

When placing the private key for a certificate at the beginning of the certificate file, you may omit the "key" item from the configuration file. The format of the combined file is:

.. parsed-literal::
   -----BEGIN RSA PRIVATE KEY-----
    [encoded key]
   -----END RSA PRIVATE KEY-----
    [empty line]
   -----BEGIN CERTIFICATE-----
    [encoded certificate]
   -----END CERTIFICATE-----
    [empty line]

**session-id-hex**

Takes a string value which is used to set the SSL session_id context for server sockets, which may be specified in the tlsid section of a config file or on WRITE/TLS("RENEGOTIATE",...). See the OpenSSL man page for SSL_set_session_id_context for usage details. The value should consist of hexadecimal digits representing the desired value. Application code can call the %UTF2HEX utility routine to translate a character string to the corresponding string of hexadecimal digits. If neither the command or the associated tlsid section in the configuration file specify a session-id-hex option when creating the socket, YottaDB uses the current tlsid, translated into hexadecimal digits.

**session-timeout**

Timeout (in seconds) for a given session. If a connection disconnects and resumes within this time interval, the session is reused to speed up the TLS handshake. A value of 0 forces sessions to not be reused. The default value is 1 hour.

**ssl-options**

The ssl_options, documented in the man page for SSL_set_options, modify the default behavior of OpenSSL. When specifying multiple options, separate them with a colon (:) delimiter. The ssl-options specified in a labeled section add to, or override, those specified at the "tls" level. An exclamation mark ("!") preceding an option in a labeled section disables any default for that option specified at the tls: level; for example:

.. parsed-literal::
   tls: {
   ssl-options: "SSL_OP_CIPHER_SERVER_PREFERENCE";
   mylabel: {
   ssl-options: "!SSL_OP_CIPHER_SERVER_PREFERENCE";
   };
   }

**verify-depth**

Certificate Authority (CA) verify depth provides an upper limit on the number of CAs to look up for verifying a given certificate. The depth count is described as "level 0:peer certificate", "level 1: CA certificate", "level 2: higher level CA certificate", and so on. The default verification depth is 9.

A verify-depth option specified in a labeled section applies to connections associated with that section.

**verify-level**

The verify-level option takes a string value to specify any additional certificate verification in addition to the basic OpenSSL verification. The only value currently accepted is "CHECK" which requests additional checks on the results of the basic OpenSSL certificate verification. A leading exclamation mark ("!") disables a verify-level option. The verify-level options specified at lower levels are merged with those options already specified at higher levels. CHECK is enabled by default for all TLS connections.

**verify-mode**

The verify-mode option specifies how OpenSSL verifies certificates. If no verify-mode is specified, it defaults to SSL_VERIFY_PEER. See the man page for SSL_set_verify for details. SSL_VERIFY_PEER has two additional flags which modify verification only for the server role; when adding them to the option string, use the colon (:) delimiter.

