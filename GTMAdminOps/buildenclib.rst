
.. index::
   Building Encryption Libraries

===========================================
Appendix D: Building Encryption Libraries
===========================================

.. contents::
   :depth: 2

-----------------------------
Building Encryption Libraries
-----------------------------

YottaDB/FIS neither encourages nor supports the use of any specific encryption library. In order to be helpful, here is how we created the libraries for testing YottaDB/GT.M in the development environment. 

++++++++++++++++++++++++++++++++++
Debian, Ubuntu, Redhat and Fedora
++++++++++++++++++++++++++++++++++

Packages were installed from standard repositories using the package manager. 

+++++++++++++++++++++++++
IBM AIX 7.1 (pSeries) 
+++++++++++++++++++++++++

**GPG-ERROR**

./configure CC=cc CFLAGS=-q64 ($OBJECT_MODE=64) 

**GCRYPT**

./configure CC="xlc -q64" --disable-asm ($OBJECT_MODE=64)/ or CC=cc CFLAGS=-q64 

**CRYPTO (from OpenSSL)**

These instructions build OpenSSL which provides libcrypto. 

.. parsed-literal::
   ./Configure aix64-cc shared # Note: it is an upper case C 
   .make 
   (as root) make install 

**GPGME**

GPGME requires a source level fix to use the proper malloc() that requires an include for stdlib.h in the include section of version.c. Then: 

.. parsed-literal::
   ./configure CC="xlc -q64" --disable-asm ($OBJECT_MODE=64)/ or CC=cc CFLAGS=-q64 

**GNUPG**

GPG on AIX requires the setuid bit to be set. This can be done via chmod u+s /path/to/gpg. Please see http://www.gnupg.org/documentation/faqs.en.html#q6.1 

.. parsed-literal::
   ./configure CC="xlc -q64" --disable-asm ($OBJECT_MODE=64) or CC=cc CFLAGS=-q64 


