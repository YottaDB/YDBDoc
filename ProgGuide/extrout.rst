
.. index::
   Integrating External Routines

===============================
Integrating External Routines
===============================

.. contents::
   :depth: 2

----------------------
Introduction
----------------------

Application code written in M can call application code written in C (or which uses a C compatible call) and vice versa.

.. note::
   This C code shares the process address space with the YottaDB/GT.M run-time library and M application code. Bugs in C code may result in difficult to diagnose failures to occur in places not obviously related to the cause of the failure.

------------------------
Access to non-M Routines
------------------------

In YottaDB/GT.M, calls to C language routines may be made with the following syntax:

.. parsed-literal::
   DO &[packagename.]name[^name][parameter-list]

or as an expression element,

.. parsed-literal::
   $&[packagename.]name[^name][parameter-list]

Where packagename, like the name elements is a valid M name. Because of the parsing conventions of M, the identifier between the ampersand (&) and the optional parameter-list has precisely constrained punctuation - a later section describes how to transform this into a more richly punctuated name should that be appropriate for the called function. While the intent of the syntax is to permit the name^name to match an M labelref, there is no semantic implication to any use of the up-arrow (^). For more information on M names, labelrefs and parameter-lists, refer toChapter 5: “General Language Features of M”.

Example:

.. parsed-literal::
   ;Call external routine rtn1
   DO &rtn1
   ;Call int^exp in package "mathpak" with one parameter: the expression val/2
   DO &mathpak.int^exp(val/2)
   ;Call the routine sqrt with the value "2"
   WRITE $&sqrt(2)
   ;Call the routine get parms, with the parameter "INPUT" and the variable "inval", passed by reference.
   DO &getparms("INPUT",.inval)
   ;Call program increment in package "mathpak" without specifying a value for the first argument and the variable "outval" passed by reference as the second argument. All arguments which do not specify a value translate to default values in the increment program.  
   Do &mathpak.increment(,.outval) 

The called routines follow the C calling conventions. They must be compiled as position independent code and linked as a shareable library.

----------------------------------
Creating a Shareable Library
----------------------------------

The method of creating a shareable library varies by the operating system. The following examples illustrate the commands on an IBM pSeries (formerly RS/6000) AIX system.

Example:

.. parsed-literal::
   $ cat increment.c
   int increment(int count, float \*invar, float \*outvar)
   {
       \*outvar=*invar+1.0;
        return 0;
   }
   $ cat decrement.c
   int decrement(int count, float \*invar, float \*outvar)
   {
    \*outvar=\*invar-1.0;
     return 0;
    }        


On IBM pSeries AIX:

Example:

.. parsed-literal::
   $ cc -c -I$gtm_dist increment.c decrement.c
   $ ld -o libcrement.so increment.o decrement.o -G -bexpall -bnoentry -bh:4 -lc

.. note::
   Refer to the AIX V4.2 documentation of the ld(1) AIX command for information on shareable libraries under AIX V4.2. 

On Linux x86:

Example:

.. parsed-literal::
   % gcc -c -fPIC -I$gtm_dist increment.c decrement.c
   % gcc -o libcrement.so -shared increment.o decrement.o

--------------------------
Using External Calls
--------------------------

The functions in programs increment and decrement are now available to YottaDB/GT.M through the shareable library libcrement.sl or libcrement.so, or though the DLL as libcrement.dll, depending on the specific platform. The suffix .sl is used throughout the following examples to represent .sl, .so, or .dll. Be sure to use the appropriate suffix for your platform.

YottaDB/GT.M uses an "external call table" to map the typeless data of M into the typed data of C, and vice versa. The external call table has a first line containing the pathname of the shareable library file followed by one or more specification lines in the following format:

.. parsed-literal::
   entryref: return-value routine-name (parameter, parameter, ... )        

where entryref is an M entryref, return-value is gtm_long_t, gtm_status_t, or void, and parameters are in the format: 

.. parsed-literal::
   direction:type [num]

where [num] indicates a pre-allocation value explained later in this chapter.

Legal directions are I, O, or IO for input, output, or input/output, respectively.

The following table describes the legal types defined in the C header file $gtm_dist/gtmxc_types.h:

**Type: Usage**

Void: Specifies that the function does not return a value.

gtm_status_t : Type int. If the function returns zero (0), then the call was successful. If it returns a non-zero value, YottaDB/GT.M will signal an error upon returning to M.

gtm_long_t : 32-bit signed integer on 32-bit platforms and 64-bit signed integer on 64-bit platforms.

gtm_ulong_t : 32-bit unsigned integer on 32-bit platforms and 64-bit signed integer on 64-bit platforms.

gtm_long_t* : For passing a pointer to long [integers].

gtm_float_t* : For passing a pointer to floating point numbers.

gtm_double_t* : Same as above, but double precision.

gtm_char_t*: For passing a "C" style string - null terminated.

gtm_char_t** : For passing a pointer to a "C" style string.

gtm_string_t* : For passing a structure in the form {int length;char \*address}. Useful for moving blocks of memory to or from YottaDB/GT.M.

gtm_pointertofunc_t : For passing callback function pointers. For details see “Callback Mechanism”.

**Note:**

If an external call's function argument is defined in the external call table, YottaDB/GT.M allows invoking that function without specifying a value of the argument. All non-trailing and output-only arguments arguments which do not specify a value translate to the following default values in C: 

* All numeric types: 0 
* gtm_char_t * and gtm_char_t \*\*: Empty string 
* gtm_string_t \*: A structure with 'length' field matching the preallocation size and 'address' field being a NULL pointer. 

In the mathpak package example, the following invocation translate inval to the default value, that is, 0.

.. parsed-literal::
   GTM>do &mathpak.increment(,.outval)

If an external call's function argument is defined in the external call table and that function is invoked without specifying the argument, ensure that the external call function appropriately handles the missing argument. As a good programming practice, always ensure that count of arguments defined in the external call table matches the function invocation. 

gtmxc_types.h also includes definitions for the following entry points exported from libgtmshr: 

.. parsed-literal::
   void gtm_hiber_start(gtm_uint_t mssleep);
   void gtm_hiber_start_wait_any(gtm_uint_t mssleep)
   void gtm_start_timer(gtm_tid_t tid, gtm_int_t time_to_expir, void (\*handler)(), gtm_int_t hdata_len, void \\\*hdata);
   void gtm_cancel_timer(gtm_tid_t tid);

where:

* mssleep - milliseconds to sleep
* tid - unique timer id value
* time_to_expir - milliseconds until timer drives given handler
* handler - function pointer to handler to be driven
* hdata_len - 0 or length of data to pass to handler as a parameter
* hdata - NULL or address of data to pass to handler as a parameter

gtm_hiber_start() always sleeps until the time expires; gtm_hiber_start_wait_any() sleeps until the time expires or an interrupt by any signal (including another timer). gtm_start_timer() starts a timer but returns immediately (no sleeping) and drives the given handler when time expires unless the timer is canceled.

.. note::
   YottaDB/GT.M continues to support xc_* equivalent types of gtm_* for upward compatibility. gtmxc_types.h explicitly marks the xc_* equivalent types as deprecated.

* Parameter-types that interface YottaDB/GT.M with non-M code using C calling conventions must match the data-types on their target platforms. Note that most addresses on 64-bit platforms are 8 bytes long and require 8 byte alignment in structures whereas all addresses on 32-bit platforms are 4 bytes long and require 4-byte alignment in structures.
* Though strings with embedded zeroes are sent as input to external routines, embedded zeroes in output (or return value) strings of type gtm_char_t may cause string truncation because they are treated as terminator.
* If your interface uses gtm_long_t or gtm_ulong_t types but your interface code uses int or signed int types, failure to revise the types so they match on a 64-bit platform will cause the code to fail in unpleasant, potentially dangerous and hard to diagnose ways.

The first parameter of each called routine is an int (for example, int argc in decrement.c and increment.c) that specifies the number of parameters passed. This parameter is implicit and only appears in the called routine. It does not appear in the call table specification, or in the M invocation. If there are no explicit parameters, the call table specification will have a zero (0) value because this value does not include itself in the count. If there are fewer actual parameters than formal parameters, the call is determined from the parameters specified by the values supplied by the M program. The remaining parameters are undefined. If there are more actual parameters than formal parameters, YottaDB/GT.M reports an error.

There may be only a single occurrence of the type gtm_status_t for each entryref.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Database Encryption Extensions to the YottaDB/GT.M External Interface
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

To support Database Encryption, YottaDB/GT.M provides a reference implementation which resides in $gtm_dist/plugin/gtmcrypt.

The reference implementation includes:

* A $gtm_dist/plugin/gtmcrypt sub-directory with all source files and scripts. The scripts include those needed to build/install libgtmcrypt.so and "helper" scripts, for example, add_db_key.sh (see below).
* The plugin interface that YottaDB/GT.M expects is defined in gtmcrypt_interface.h. Never modify this file - it defines the interface that the plugin must provide.
* $gtm_dist/plugin/libgtmcrypt.so is the shared library containing the executables which is dynamically linked by YottaDB/GT.M and which in turn calls the encryption packages. If the $gtm_dist/utf8 directory exists, then it should contain a symbolic link to ../plugin.
* Source code is provided in the file $gtm_dist/plugin/gtmcrypt/source.tar which includes build.sh and install.sh scripts to respectively compile and install libgtmcrypt.so from the source code.

To support the implementation of a reference implementation, YottaDB/GT.M provides additional C structure types (in the gtmxc_types.h file):

* gtmcrypt_key_t - a datatype that is a handle to a key. The YottaDB/GT.M database engine itself does not manipulate keys. The plug-in keeps the keys, and provides handles to keys that the YottaDB/GT.M database engine uses to refer to keys.
* xc_fileid_ptr_t - a pointer to a structure maintained by YottaDB/GT.M to uniquely identify a file. Note that a file may have multiple names - not only as a consequence of absolute and relative path names, but also because of symbolic links and also because a file system can be mounted at more than one place in the file name hierarchy. YottaDB/GT.M needs to be able to uniquely identify files.

Although not required to be used by a customized plugin implementation, YottaDB/GT.M provides (and the reference implementation uses) the following functions for uniquely identifying files:

* xc_status_t gtm_filename_to_id(xc_string_t \*filename, xc_fileid_ptr_t \*fileid) - function that takes a file name and provides the file id structure for that file.
* xc_status_t gtm_is_file_identical(xc_fileid_ptr_t fileid1, xc_fileid_ptr_t fileid2) - function that determines whether two file ids map to the same file.
* gtm_xcfileid_free(xc_fileid_ptr_t fileid) - function to release a file id structure.

Mumps, MUPIP and DSE processes dynamically link to the plugin interface functions that reside in the shared library. The functions serve as software "shims" to interface with an encryption library such as libmcrypt or libgpgme / libgcrypt.

The plugin interface functions are:

* gtmcrypt_init()
* gtmcrypt_getkey_by_name()
* gtmcrypt_getkey_by_hash()
* gtmcrypt_hash_gen()
* gtmcrypt_encode()
* gtmcrypt_decode()
* gtmcrypt_close()
* and gtmcrypt_strerror()

A YottaDB/GT.M database consists of multiple database files, each of which has its own encryption key, although you can use the same key for multiple files. Thus, the gtmcrypt* functions are capable of managing multiple keys for multiple database files. Prototypes for these functions are in gtmcrypt_interface.h.

The core plugin interface functions, all of which return a value of type gtm_status_t are:

* gtmcrypt_init() performs initialization. If the environment variable $gtm_passwd exists and has an empty string value, YottaDB/GT.M calls gtmcrypt_init() before the first M program is loaded; otherwise it calls gtmcrypt_init() when it attempts the first operation on an encrypted database file.
* Generally, gtmcrypt_getkey_by_hash or, for MUPIP CREATE, gtmcrypt_getkey_by_name perform key acquisition, and place the keys where gtmcrypt_decode() and gtmcrypt_encode() can find them when they are called.
* Whenever YottaDB/GT.M needs to decode a block of bytes, it calls gtmcrypt_decode() to decode the encrypted data. At the level at which GT.M database encryption operates, it does not matter what the data is - numeric data, string data whether in M or UTF-8 mode and whether or not modified by a collation algorithm. Encryption and decryption simply operate on a series of bytes.
* Whenever YottaDB/GT.M needs to encode a block of bytes, it calls gtmcrypt_encode() to encode the data.
* If encryption has been used (if gtmcrypt_init() was previously called and returned success), YottaDB/GT.M calls gtmcrypt_close() at process exit and before generating a core file. gtmcrypt_close() must erase keys in memory to ensure that no cleartext keys are visible in the core file.

More detailed descriptions follow.

* gtmcrypt_key_t \*gtmcrypt_getkey_by_name(gtm_string_t \*filename) - MUPIP CREATE uses this function to get the key for a database file. This function searches for the given filename in the memory key ring and returns a handle to its symmetric cipher key. If there is more than one entry for the given filename , the reference implementation returns the entry matching the last occurrence of that filename in the master key file.
* gtm_status_t gtmcrypt_hash_gen(gtmcrypt_key_t \*key, gtm_string_t \*hash) - MUPIP CREATE uses this function to generate a hash from the key then copies that hash into the database file header. The first parameter is a handle to the key and the second parameter points to 256 byte buffer. In the event the hash algorithm used provides hashes smaller than 256 bytes, gtmcrypt_hash_gen() must fill any unused space in the 256 byte buffer with zeros.
* gtmcrypt_key_t \*gtmcrypt_getkey_by_hash(gtm_string_t \*hash) - YottaDB/GT.M uses this function at database file open time to obtain the correct key using its hash from the database file header. This function searches for the given hash in the memory key ring and returns a handle to the matching symmetric cipher key. MUPIP LOAD, MUPIP RESTORE, MUPIP EXTRACT, MUPIP JOURNAL and MUPIP BACKUP -BYTESTREAM all use this to find keys corresponding to the current or prior databases from which the files they use for input were derived.
* gtm_status_t gtmcrypt_encode(gtmcrypt_key_t \*key, gtm_string_t \*inbuf, gtm_string_t \*outbuf) and gtm_status_t gtmcrypt_decode(gtmcrypt_key_t \*key, gtm_string_t \*inbuf, gtm_string_t \*outbuf)- YottaDB/GT.M uses these functions to encode and decode data. The first parameter is a handle to the symmetric cipher key, the second a pointer to the block of data to encode or decode, and the third a pointer to the resulting block of encoded or decoded data. Using the appropriate key (same key for a symmetric cipher), gtmcrypt_decode() must be able to decode any data buffer encoded by gtmcrypt_encode(), otherwise the encrypted data is rendered unrecoverable. As discussed earlier, YottaDB/GT.M requires the encrypted and cleartext versions of a string to have the same length.
* char \*gtmcrypt_strerror() - YottaDB/GT.M uses this function to retrieve addtional error context from the plug-in after the plug-in returns an error status. This function returns a pointer to additional text related to the last error that occurred. YottaDB/GT.M displays this text as part of an error report. In a case where an error has no additional context or description, this function returns a null string.

The complete source code for reference implementations of these functions is provided, licensed under the same terms as YottaDB/GT.M. You are at liberty to modify them to suit your specific YottaDB/GT.M database encryption needs. Check your YottaDB/GT.M license if you wish to consider redistributing your changes to others.

For more information and examples, refer to the Database Encryption Technical Bulletin.

++++++++++++++++++++++++++++++++++++
Pre-allocation of Output Parameters
++++++++++++++++++++++++++++++++++++

The definition of parameters passed by reference with direction output can include specification of a pre-allocation value. This is the number of units of memory that the user wants YottaDB/GT.M to allocate before passing the parameter to the external routine. For example, in the case of type gtm_char_t \*, the pre-allocation value would be the number of bytes to be allocated before the call to the external routine.

Specification of a pre-allocation value should follow these rules:

* Pre-allocation is an unsigned integer value specifying the number of bytes to be allocated on the system heap with a pointer passed into the external call.
* Pre-allocating on a type with a direction of input or input/output results in a YottaDB/GT.M error.
* Pre-allocation is meaningful only on types gtm_char_t * and gtm_string_t \*. On all other types the pre-allocation value specified will be ignored and the parameter will be allocated a default value for that type. With gtm_string_t * arguments make sure to set the 'length' field appropriately before returning control to YottaDB/GT.M. On return from the external call, YottaDB/GT.M uses the value in the length field as the length of the returned value, in bytes.
* If the user does not specify any value, then the default pre-allocation value would be assigned to the parameter.
* Specification of pre-allocation for "scalar" types (parameters which are passed by value) is an error.

.. note::
   Pre-allocation is optional for all output-only parameters except gtm_string_t * and gtm_char_t \*. Pre-allocation yields better management of memory for the external call. 

+++++++++++++++++++++++++++++
Callback Mechanism
+++++++++++++++++++++++++++++

YottaDB/GT.M exposes certain functions that are internal to the YottaDB/GT.M runtime library for the external calls via a callback mechanism. While making an external call, YottaDB/GT.M populates and exposes a table of function pointers containing addresses to call-back functions.

+----------+---------------------+--------------------+--------------------+----------------------------------------------------------------------------------------------------------------------------+
| Index    | Function            | Argument           | Type               | Description                                                                                                                |
+==========+=====================+====================+====================+============================================================================================================================+
| 0        | hiber_start         |                    |                    | sleep for a specified time                                                                                                 |
+----------+---------------------+--------------------+--------------------+----------------------------------------------------------------------------------------------------------------------------+
|          |                     | slp_time           | integer            | milliseconds to sleep                                                                                                      |
+----------+---------------------+--------------------+--------------------+----------------------------------------------------------------------------------------------------------------------------+
| 1        | hiber_start_wait_any|                    |                    | sleep for a specified time or until any interrupt, whichever comes first                                                   |
+----------+---------------------+--------------------+--------------------+----------------------------------------------------------------------------------------------------------------------------+
|          |                     | slp_time           | integer            | milliseconds to sleep                                                                                                      |
+----------+---------------------+--------------------+--------------------+----------------------------------------------------------------------------------------------------------------------------+
| 2        | start_timer         |                    |                    | start a timer and invoke a handler function when the timer expires                                                         |
+----------+---------------------+--------------------+--------------------+----------------------------------------------------------------------------------------------------------------------------+
|          |                     | tid                | integer            | unique user specified identifier for this timer                                                                            |
+----------+---------------------+--------------------+--------------------+----------------------------------------------------------------------------------------------------------------------------+
|          |                     | time_to_expire     | integer            | milliseconds before handler is invoked                                                                                     |
+----------+---------------------+--------------------+--------------------+----------------------------------------------------------------------------------------------------------------------------+
|          |                     | handler            | pointer to function| specifies the entry of the handler function to invoke                                                                      |
+----------+---------------------+--------------------+--------------------+----------------------------------------------------------------------------------------------------------------------------+
|          |                     | hlen               | integer            | length of data to be passed via the hdata argument                                                                         |
+----------+---------------------+--------------------+--------------------+----------------------------------------------------------------------------------------------------------------------------+
|          |                     | hdata              | pointer to char    | data (if any) to pass to the handler function                                                                              |
+----------+---------------------+--------------------+--------------------+----------------------------------------------------------------------------------------------------------------------------+
| 3        | cancel_timer        |                    |                    | stop a timer previously started with start_timer(), if it has not yet expired                                              |
+----------+---------------------+--------------------+--------------------+----------------------------------------------------------------------------------------------------------------------------+
|          |                     | tid                | integer            | unique user specified identifier of the timer to cancel                                                                    |
+----------+---------------------+--------------------+--------------------+----------------------------------------------------------------------------------------------------------------------------+
| 4        | gtm_malloc          |                    |                    | allocates process memory from the heap                                                                                     |
+----------+---------------------+--------------------+--------------------+----------------------------------------------------------------------------------------------------------------------------+
|          |                     | <return-value>     | pointer to void    | address of the allocated space                                                                                             |
+----------+---------------------+--------------------+--------------------+----------------------------------------------------------------------------------------------------------------------------+
|          |                     | space needed       | 32-bit platforms:  | bytes of space to allocate. This has the same signature as the system malloc() call.                                       |
|          |                     |                    | 32-bit unsigned    |                                                                                                                            |
|          |                     |                    | integer            |                                                                                                                            |
|          |                     |                    |                    |                                                                                                                            |
|          |                     |                    | 64-bit platforms:  |                                                                                                                            |
|          |                     |                    | 64-bit unsigned    |                                                                                                                            |
|          |                     |                    | integer            |                                                                                                                            |
+----------+---------------------+--------------------+--------------------+----------------------------------------------------------------------------------------------------------------------------+
| 5        | gtm_free            |                    |                    | return memory previously allocated with gtm_malloc()                                                                       |
+----------+---------------------+--------------------+--------------------+----------------------------------------------------------------------------------------------------------------------------+
|          |                     | free_address       | pointer to void    | address of the previously allocated space                                                                                  |
+----------+---------------------+--------------------+--------------------+----------------------------------------------------------------------------------------------------------------------------+

The external routine can access and invoke a call-back function in any of the following mechanisms: 

* While making an external call, YottaDB/GT.M sets the environment variable GTM_CALLIN_START to point to a string containing the start address (decimal integer value) of the table described above. The external routine needs to read this environment variable, convert the string into an integer value and should index into the appropriate entry to call the appropriate YottaDB/GT.M function.
* YottaDB/GT.M also provides an input-only parameter type gtm_pointertofunc_t that can be used to obtain call-back function pointers via parameters in the external routine. If a parameter is specified as I:gtm_pointertofunc_t and if a numeric value (between 0-5) is passed for this parameter in M, YottaDB/GT.M interprets this value as the index into the callback table and passes the appropriate callback function pointer to the external routine.

.. note::
   YottaDB/FIS strongly discourages the use of signals, especially SIGALARM, in user written C functions. YottaDB/GT.M assumes that it has complete control over any signals that occur and depends on that behavior for recovery if anything should go wrong. The use of exposed timer APIs should be considered for timer needs.

++++++++++++++++++++++++++++++++++++
Limitations on the External Program
++++++++++++++++++++++++++++++++++++

Since both YottaDB/GT.M runtime environment and the external C functions execute in the same process space, the following restrictions apply to the external functions:

* YottaDB/GT.M is designed to use signals and has signal handlers that must function for YottaDB/GT.M to operate properly. The timer related call-backs should be used in place of any library or system call which uses SIGALRM such as sleep(). Use of signals by external call code may cause YottaDB/GT.M to fail.
* Use of the YottaDB/GT.M provided malloc and free, creates an integrated heap management system, which has a number of debugging tools. YottaDB/FIS recommends the usage of gtm_malloc/gtm_free in the external functions that provides better debugging capability in case memory management problems occur with external calls.
* Use of exit system call in external functions is strongly discouraged. Since YottaDB/GT.M uses exit handlers to properly shutdown runtime environment and any active resources, the system call _exit should never be used in external functions.
* YottaDB/GT.M uses timer signals so often that the likelihood of a system call being interrupted is high. So, all system calls in the external program can return EINTR if interrupted by a signal.
* Handler functions invoked with start_timer must not invoke services that are identified by the Operating System documentation as unsafe for signal handlers (or not identified as safe) - consult the system documentation or man pages for this information. Such services cause non-deterministic failures when they are interrupted by a function that then attempts to call them, wrongly assuming they are reentrant.

++++++++++++++++++++++++++++++++++++++++
Examples of Using External Calls
++++++++++++++++++++++++++++++++++++++++

.. parsed-literal::
   foo: void bar (I:gtm_float_t*, O:gtm_float_t*)

There is one external call table for each package. The environment variable "GTMXC" must name the external call table file for the default package. External call table files for packages other than the default must be identified by environment variables of the form "GTMXC_name".

The first of the external call tables is the location of the shareable library. The location can include environment variable names.

Example: 

.. parsed-literal::
   % echo $GTMXC_mathpak
   /user/joe/mathpak.xc
   % echo lib /usr/
   % cat mathpak.xc
   $lib/mathpak.sl
   exp: gtm_status_t xexp(I:gtm_float_t*, O:gtm_float_t*)
   % cat exp.c
   ...
   int xexp(count, invar, outvar)
   int count;
   float \*invar;
   float \*outvar;
   {
    ...
   }
   % gtm
   ... 
   GTM>d &mathpak.exp(inval,.outval)
   GTM>

Example : For preallocation: 

.. parsed-literal::
   % echo $GTMXC_extcall
   /usr/joe/extcall.xc
   % cat extcall.xc
   /usr/lib/extcall.sl
   prealloc: void gtm_pre_alloc_a(O:gtm_char_t \*[12])
   % cat extcall.c
   #include <stdio.h>
   #include <string.h>
   #include "gtmxc_types.h"
   void gtm_pre_alloc_a (int count, char \*arg_prealloca)
   {
    strcpy(arg_prealloca, "New Message");
    return;
   }

Example : for call-back mechanism

.. parsed-literal::
   % echo $GTMXC 
   /usr/joe/callback.xc 
   % cat /usr/joe/callback.xc 
   $MYLIB/callback.sl 
   init:     void   init_callbacks() 
   tstslp:  void   tst_sleep(I:gtm_long_t) 
   strtmr: void   start_timer(I:gtm_long_t, I:gtm_long_t) 
   % cat /usr/joe/callback.c 
   #include <stdio.h> 
   #include <stdlib.h> 
    
   #include "gtmxc_types.h" 
 
   void \*\*functable; 
   void (\*setup_timer)(int , int , void (*)() , int , char \*); 
   void (\*cancel_timer)(int ); 
   void (\*sleep_interrupted)(int ); 
   void (\*sleep_uninterrupted)(int ); 
   void* (\*malloc_fn)(int); 
   void (\*free_fn)(void*); 
 
   void  init_callbacks (int count) 
   { 
      char \*start_address; 
    
      start_address = (char \*)getenv("GTM_CALLIN_START"); 
       
      if (start_address == (char \*)0) 
       { 
        fprintf(stderr,"GTM_CALLIN_START is not set\n"); 
        return; 
       } 
      functable = (void \*\*)atoi(start_address); 
      if (functable == (void \*\*)0) 
      { 
       perror("atoi : "); 
       fprintf(stderr,"addresses defined by GTM_CALLIN_START not a number\n"); 
       return; 
      } 
      sleep_uninterrupted = (void (*)(int )) functable[0]; 
      sleep_interrupted = (void (*)(int )) functable[1]; 
      setup_timer = (void (*)(int , int, void (*)(), int, char \*)) functable[2]; 
      cancel_timer = (void (*)(int )) functable[3]; 
                                                                      
      malloc_fn = (void* (*)(int)) functable[4]; 
      free_fn = (void (*)(void*)) functable[5]; 
                                                                              
      return; 
   } 
                                                                     
   void  sleep (int count, int time) 
   { 
      (\*sleep_uninterrupted)(time); 
   } 
                                                                                    
   void timer_handler () 
   { 
      fprintf(stderr,"Timer Handler called\n"); 
      /* Do something \*/ 
   } 
                                                                                          
   void  start_timer (int count, int time_to_int, int time_to_sleep) 
   { 
      (\*setup_timer)((int )start_timer, time_to_int, timer_handler, 0, 0); 
      return; 
   } 
   void* xmalloc (int count) 
   {   
     return (\*malloc_fn)(count); 
   } 
                                                                                                
   void  xfree(void* ptr) 
   { 
     (\*free_fn)(ptr); 
   }

Example:gtm_malloc/gtm_free callbacks using gtm_pointertofunc_t

.. parsed-literal::
   % echo $GTMXC
   /usr/joe/callback.xc
   % cat /usr/joe/callback.xc
   /usr/lib/callback.sl
   init: void init_callbacks(I:gtm_pointertofunc_t, I:gtm_pointertofunc_t)
   % gtm
   GTM> do &.init(4,5)
   GTM>
   % cat /usr/joe/callback.c
   #include <stdio.h>
   #include <stdlib.h>
   #include "gtmxc_types.h"
   void* (\*malloc_fn)(int);
   void (\*free_fn)(void*);
   void init_callbacks(int count, void* (\*m)(int), void (\*f)(void*))
   {
       malloc_fn = m;
       free_fn = f;
   }

-----------------------------------------
Calls from External Routines: Call-Ins
-----------------------------------------

Call-In is a framework supported by YottaDB/GT.M that allows a C/C++ program to invoke an M routine within the same process context. YottaDB/GT.M provides a well-defined Call-In interface packaged as a run-time shared library that can be linked into an external C/C++ program.

+++++++++++++++++++++++++++
Relevant Files for Call-Ins
+++++++++++++++++++++++++++

To facilitate Call-Ins to M routines, the YottaDB/GT.M distribution directory ($gtm_dist) contains the following files:

* libgtmshr.so - A shared library that implements the YottaDB/GT.M run-time system, including the Call-In API. If Call-Ins are used from a standalone C/C++ program, this library needs to be explicitly linked into the program. See “Building Standalone Programs”, which describes the necessary linker options on each supported platforms.
* mumps - The YottaDB/GT.M startup program that dynamically links with libgtmshr.so.
* gtmxc_types.h - A C-header file containing the declarations of Call-In API.

.. note::
   .so is the recognized shared library file extension on most UNIX platforms.

The following sections describe the files relevant to using Call-Ins.

**gtmxc_types.h**

The header file provides signatures of all Call-In interface functions and definitions of those valid data types that can be passed from C to M. YottaDB/FIS strongly recommends that these types be used instead of native types (int, char, float, and so on), to avoid possible mismatch problems during parameter passing.

gtmxc_types.h defines the following types that can be used in Call-Ins.

+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Type                  | Usage                                                                                                                                                        |
+=======================+==============================================================================================================================================================+
| void                  | Used to express that there is no function return value                                                                                                       |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
| gtm_int_t             | gtm_int_t has 32-bit length on all platforms.                                                                                                                |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
| gtm_uint_t            | gtm_uint_t has 32-bit length on all platforms                                                                                                                |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
| gtm_long_t            | gtm_long_t has 32-bit length on 32-bit platforms and 64-bit length on 64-bit platforms. It is much the same as the C language long type.                     |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
| gtm_ulong_t           | gtm_ulong_t is much the same as the C language unsigned long type.                                                                                           |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
| gtm_float_t           | floating point number                                                                                                                                        |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
| gtm_double_t          | Same as above but double precision.                                                                                                                          |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
| gtm_status_t          | type int. If it returns zero then the call was successful. If it is non-zero, when control returns to YottaDB/GT.M, it issues a trappable error.             |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
| gtm_long_t*           | Pointer to gtm_long_t. Good for returning integers.                                                                                                          |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
| gtm_ulong_t*          | Pointer to gtm_ulong_t. Good for returning unsigned integers.                                                                                                |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+

.. parsed-literal::
   typedef struct {
       gtm_long_t length;
       gtm_char_t* address;
   } gtm_string_t;

The pointer types defined above are 32-bit addresses on all 32-bit platforms. For 64-bit platforms, gtm_string_t* is a 64-bit address.

gtmxc_types.h also provides an input-only parameter type gtm_pointertofunc_t that can be used to obtain call-back function pointers via parameters in the external routine. If a parameter is specified as I:gtm_pointertofunc_t and if a numeric value (between 0-5) is passed for this parameter in M, YottaDB/GT.M interprets this value as the index into the callback table and passes the appropriate callback function pointer to the external routine.

.. note::
   YottaDB/GT.M represents values that fit in 18 digits as numeric values, and values that require more than 18 digits as strings.

gtmxc_types.h also includes definitions for the following entry points exported from libgtmshr: 

.. parsed-literal::
   void gtm_hiber_start(gtm_uint_t mssleep);
   void gtm_hiber_start_wait_any(gtm_uint_t mssleep)
   void gtm_start_timer(gtm_tid_t tid, gtm_int_t time_to_expir, void (\*handler)(), gtm_int_t hdata_len, void \\*hdata);
   void gtm_cancel_timer(gtm_tid_t tid);

where:

* mssleep - milliseconds to sleep
* tid - unique timer id value
* time_to_expir - milliseconds until timer drives given handler
* handler - function pointer to handler to be driven
* hdata_len - 0 or length of data to pass to handler as a parameter
* hdata - NULL or address of data to pass to handler as a parameter

gtm_hiber_start() always sleeps until the time expires; gtm_hiber_start_wait_any() sleeps until the time expires or an interrupt by any signal (including another timer). gtm_start_timer() starts a timer but returns immediately (no sleeping) and drives the given handler when time expires unless the timer is canceled.

.. note::
   YottaDB/GT.M continues to support xc_* equivalent types of gtm_* for upward compatibility. gtmxc_types.h explicitly marks the xc_* equivalent types as deprecated.

**Call-In table**

The Call-In table file is a text file that contains the signatures of all M label references that get called from C. In order to pass the typed C arguments to the type-less M formallist, the enviroment variable GTMCI must be defined to point to the Call-In table file path. Each signature must be specified separately in a single line. YottaDB/GT.M reads this file and interprets each line according to the following convention (specifications within box brackets "[]", are optional):

.. parsed-literal::
   <c-call-name> : <ret-type> <label-ref> ([<direction>:<param-type>,...])

where,

<label-ref>: is the entry point (that is a valid label reference) at which YottaDB/GT.M starts executing the M routine being called-in

<c-call-name>: is a unique C identifier that is actually used within C to refer to <label-ref>

<direction>: is either I (input-only), O (output-only), or IO (input-output)

<ret-type>: is the return type of <label-ref>

.. note::
   Since the return type is considered as an output-only (O) parameter, the only types allowed are pointer types and void. Void cannot be specified as parameter.

<param-type>: is a valid parameter type. Empty parentheses must be specified if no argument is passed to <label-ref>

The <direction> indicates the type of operation that YottaDB/GT.M performs on the parameter read-only (I), write-only (O), or read-write (IO). All O and IO parameters must be passed by reference, that is as pointers since YottaDB/GT.M writes to these locations. All pointers that are being passed to YottaDB/GT.M must be pre-allocated. The following table details valid type specifications for each direction.

+-------------------+---------------------------------------------------------------------------------------------------------------------------------------------+
| Directions        | Allowed Parameter Types                                                                                                                     |
+===================+=============================================================================================================================================+
| I                 | gtm_long_t, gtm_ulong_t, gtm_float_t, gtm_double_t,_gtm_long_t*, gtm_ulong_t*, gtm_float_t*, gtm_double_t*,_gtm_char_t*, gtm_string_t*      |
+-------------------+---------------------------------------------------------------------------------------------------------------------------------------------+
| O/IO              | gtm_long_t*, gtm_ulong_t*, gtm_float_t*, gtm_double_t*,_gtm_char_t*, gtm_string_t*                                                          |
+-------------------+---------------------------------------------------------------------------------------------------------------------------------------------+

Here is an example of Call-In table (calltab.ci) for piece.m (see “Example: Calling GT.M from a C Program”):

.. parsed-literal::
   print     :void            display^piece()
   getpiece  :gtm_char_t*     get^piece(I:gtm_char_t*, I:gtm_char_t*, I:gtm_long_t)
   setpiece  :void            set^piece(IO:gtm_char_t*, I:gtm_char_t*, I:gtm_long_t, I:gtm_char_t*)
   pow       :gtm_double_t*   pow^piece(I:gtm_double_t, I:gtm_long_t)
   powequal  :void            powequal^piece(IO:gtm_double_t*, I:gtm_long_t)
   piece     :gtm_double_t*   pow^piece(I:gtm_double_t, I:gtm_long_t)

.. note::
   The same entryref can be called by different C call names (for example, pow, and piece). However, if there are multiple lines with the same call name, only the first entry will be used by YottaDB/GT.M. YottaDB/GT.M ignores all subsequent entries using a call name. Also, note that the second and third entries, although shown here as wrapped across lines, must be specified as a single line in the file.

++++++++++++++++++++++++
Call-In Interface
++++++++++++++++++++++++

This section is further broken down into 6 subsections for an easy understanding of the Call-In interface. The section is concluded with an elaborate example.

**Initialize YottaDB/GT.M**

.. parsed-literal::
   gtm_status_t gtm_init(void);

If the base program is not an M routine but a standalone C program, gtm_init() must be called (before calling any YottaDB/GT.M functions), to initialize the YottaDB/GT.M run-time system.

gtm_init() returns zero (0) on success. On failure, it returns the YottaDB/GT.M error status code whose message can be read into a buffer by immediately calling gtm_zstatus() (see “Print Error Messages”). Duplicate invocations of gtm_init() are ignored by YottaDB/GT.M.

If Call-Ins are used from an external call function (that is, a C function that has itself been called from M code), gtm_init() is not needed, because YottaDB/GT.M is initialized before the External Call. All gtm_init() calls from External Calls functions are ignored by YottaDB/GT.M.

**Call an M Routine from C**

YottaDB/GT.M provides 2 interfaces for calling a M routine from C. These are:

* gtm_cip
* gtm_ci

gtm_cip offers better performance on calls after the first one. 

**gtm_cip**

.. parsed-literal::
   gtm_status_t gtm_cip(ci_name_descriptor \*ci_info, ...);

The variable argument function gtm_cip() is the interface that invokes the specified M routine and returns the results via parameters.

ci_name_descriptor has the following structure:

.. parsed-literal::
   typedef struct
   {
     gtm_string_t rtn_name;
     void* handle;
   } ci_name_descriptor;

rtn_name is a C character string indicating the corresponding <lab-ref> entry in the Call-In table.

The handle is YottaDB/GT.M private information initialized by YottaDB/GT.M on the first call-in and to be provided unmodified to YottaDB/GT.M on subsequent calls. If application code modifies it, it will corrupt the address space of the process, and potentially cause just about any bad behavior that it is possible for the process to cause, including but not limited to process death, database damage and security violations.

The gtm_cip() call must follow the following format:

.. parsed-literal::
   status = gtm_cip(<ci_name_descriptor> [, ret_val] [, arg1] ...);

First argument: ci_name_descriptor, a null-terminated C character string indicating the alias name for the corresponding <lab-ref> entry in the Call-In table.

Optional second argument: ret_val, a pre-allocated pointer through which YottaDB/GT.M returns the value of QUIT argument from the (extrinsic) M routine. ret_val must be the same type as specified for <ret-type> in the Call-In table entry. The ret_val argument is needed if and only if <ret-type> is not void.

Optional list of arguments to be passed to the M routine's formallist: the number of arguments and the type of each argument must match the number of parameters, and parameter types specified in the corresponding Call-In table entry. All pointer arguments must be pre-allocated. YottaDB/GT.M assumes that any pointer, which is passed for O/IO-parameter points to valid write-able memory.

The status value returned by gtm_cip() indicates the YottaDB/GT.M status code; zero (0), if successful, or a non-zero; $ZSTATUS error code on failure. The $ZSTATUS message of the failure can be read into a buffer by immediately calling gtm_zstatus() (for details, see “Print Error Messages”).

**gtm_ci**

.. parsed-literal::
   gtm_status_t gtm_ci(const gtm_char_t* c_call_name, ...);

The variable argument function gtm_ci() is the interface that actually invokes the specified M routine and returns the results via parameters. The gtm_ci() call must be in the following format:

.. parsed-literal::
   status = gtm_ci(<c_call_name> [, ret_val] [, arg1] ...);

First argument: c_call_name, a null-terminated C character string indicating the alias name for the corresponding <lab-ref> entry in the Call-In table.

Optional second argument: ret_val, a pre-allocated pointer through which YottaDB/GT.M returns the value of QUIT argument from the (extrinsic) M routine. ret_val must be the same type as specified for <ret-type> in the Call-In table entry. The ret_val argument is needed if and only if <ret-type> is not void.

Optional list of arguments to be passed to the M routine's formallist: the number of arguments and the type of each argument must match the number of parameters, and parameter types specified in the corresponding Call-In table entry. All pointer arguments must be pre-allocated. YottaDB/GT.M assumes that any pointer, which is passed for O/IO-parameter points to valid write-able memory.

The status value returned by gtm_ci() indicates the YottaDB/GT.M status code; zero (0), if successful, or a non-zero; $ZSTATUS error code on failure. The $ZSTATUS message of the failure can be read into a buffer by immediately calling gtm_zstatus(). For more details, see “Print Error Messages”.

**Example: Calling YottaDB/GT.M from a C Program**

Here are some working examples of C programs that use call-ins to invoke YottaDB/GT.M. Each example is packaged in a zip file which contains a C program, a call-in table, and a YottaDB/GT.M API. To run an example, download and follow the compiling and linking instructions in the comments of the C program.

+--------------------------------+----------------------------------------------------------------------------------------------+
| Example                        | Download Information                                                                         |
+================================+==============================================================================================+
| gtmaccess.c (gtm_ci interface) | http://tinco.pair.com/bhaskar/gtm/doc/books/pg/UNIX_manual/gtmci_gtmaccess.zip               |
+--------------------------------+----------------------------------------------------------------------------------------------+
| gtmaccess.c (gtm_cip interface)| http://tinco.pair.com/bhaskar/gtm/doc/books/pg/UNIX_manual/gtmcip_gtmaccess.zip              |
+--------------------------------+----------------------------------------------------------------------------------------------+
| cpiece.c (gtm_ci interface)    | http://tinco.pair.com/bhaskar/gtm/doc/books/pg/UNIX_manual/gtmci_cpiece.zip                  |
+--------------------------------+----------------------------------------------------------------------------------------------+

**Print Error Messages**

.. parsed-literal::
   void gtm_zstatus (gtm_char_t* msg_buffer, gtm_long_t buf_len);

This function returns the null-terminated $ZSTATUS message of the last failure via the buffer pointed by msg_buffer of size buf_len. The message is truncated to size buf_len if it does not fit into the buffer. gtm_zstatus() is useful if the external application needs the text message corresponding to the last YottaDB/GT.M failure. A buffer of 2048 is sufficient to fit in any YottaDB/GT.M message.

**Exit from YottaDB/GT.M**

.. parsed-literal::
   gtm_status_t  gtm_exit (void);

gtm_exit() can be used to shut down all databases and exit from the YottaDB/GT.M environment that was created by a previous gtm_init().

Note that gtm_init() creates various YottaDB/GT.M resources and keeps them open across multiple invocations of gtm_ci() until gtm_exit() is called to close all such resources. On successful exit, gtm_exit() returns zero (0), else it returns the $ZSTATUS error code.

gtm_exit() cannot be called from an external call function. YottaDB/GT.M reports the error GTM-E-INVGTMEXIT if an external call function invokes gtm_exit(). Since the YottaDB/GT.M run-time system must be operational even after the external call function returns, gtm_exit() is meant to be called only once during a process lifetime, and only from the base C/C++ program when YottaDB/GT.M functions are no longer required by the program.

+++++++++++++++++++++++++++++
Building Standalone Programs
+++++++++++++++++++++++++++++

All external C functions that use call-ins should include the header file gtmxc_types.h that defines various types and provides signatures of call-in functions. To avoid potential size mismatches with the parameter types, YottaDB/FIS strongly recommends that gtm \*t types defined in gtmxc_types.h be used instead of the native types (int, float, char, etc).

To use call-ins from a standalone C program, it is necessary that the YottaDB/GT.M runtime library (libgtmshr.so) is explicitly linked into the program. If call-ins are used from an External Call function (which in turn was called from YottaDB/GT.M through the existing external call mechanism), the External Call library does not need to be linked explicitly with libgtmshr.so since YottaDB/GT.M would have already loaded it.

The following sections describe compiler and linker options that must be used on each platform for call-ins to work from a standalone C/C++ program. 

**IBM pSeries (RS/6000) AIX**

* Compiler: -I$gtm_dist
* Linker: -L$gtm_dist -lgtmshr

**X86 GNU/Linux**

* Compiler: -I$gtm_dist
* Linker: -L$gtm_dist -lgtmshr -rpath $gtm_dist
* YottaDB/FIS advises that the C/C++ compiler front-end be used as the Linker to avoid specifying the system startup routines on the ld command. The compile can pass linker options to ld using -W option (for example, cc -W1, -R, $gtm_dist). For more details on these options, refer to the appropriate system's manual on the respective platforms.

++++++++++++++++++++++++++++++
Nested Call-Ins
++++++++++++++++++++++++++++++

Call-ins can be nested by making an external call function in-turn call back into YottaDB/GT.M. Each gtm_ci() called from an External Call library creates a call-in base frame at $ZLEVEL 1 and executes the M routine at $ZLEVEL 2. The nested call-in stack unwinds automatically when the External Call function returns to YottaDB/GT.M.

YottaDB/GT.M currently allows up to 10 levels of nesting, if TP is not used, and less than 10 if YottaDB/GT.M supports call-ins from a transaction (see “Rules to Follow in Call-Ins”). YottaDB/GT.M reports the error GTM-E-CIMAXLEVELS when the nesting reaches its limit.

Following are the YottaDB/GT.M commands, Intrinsic Special Variables, and functions whose behavior changes in the context of every new nested call-in environment.

ZGOTO operates only within the current nested M stack. ZGOTO zero (0) unwinds all frames in the current nested call-in M stack (including the call-in base frame) and returns to C. ZGOTO one (1) unwinds all current stack frame levels up to (but not inclusive) the call-in base frame and returns to C, while keeping the current nested call-in environment active for any following gtm_ci() calls.

$ZTRAP/$ETRAP NEW'd at level 1 (in GTM$CI frame).

$ZLEVEL initializes to one (1) in GTM$CI frame, and increments for every new stack level.

$STACK initializes to zero (0) in GTM$CI frame, and increments for every new stack level.

$ESTACK NEW'd at level one (1) in GTM$CI frame.

$ECODE/$STACK() initialized to null at level one (1) in GTM$CI frame.

.. note::
   After a nested call-in environment exits and the external call C function returns to M, the above ISVs and Functions restore their old values.

++++++++++++++++++++++++++++++++++++
Rules to Follow in Call-Ins
++++++++++++++++++++++++++++++++++++

1. External calls must not be fenced with TSTART/TCOMMIT if the external routine calls back into mumps using call-in mechanism. YottaDB/GT.M reports the error GTM-E-CITPNESTED if nested call-ins are invoked within a TP fence since YottaDB/GT.M currently does not handle TP support across multiple call-in invocations.
2. The external application should never call exit() unless it has called gtm_exit() previously. YottaDB/GT.M internally installs an exit handler that should never be bypassed.
3. The external application should never use any signals when YottaDB/GT.M is active since YottaDB/GT.M reserves them for its internal use. YottaDB/GT.M provides the ability to handle SIGUSR1 within M (see “$ZINTerrupt” for more information). An interface is provided by YottaDB/GT.M for timers. Although not required, YottaDB/FIS recommends the use of gtm_malloc() and gtm_free() for memory management by C code that executes in a YottaDB/GT.M process space for enhanced performance and improved debugging.
4. YottaDB/GT.M performs device input using the read() system service. UNIX documentation recommends against mixing this type of input with buffered input services in the fgets() family and ignoring this recommendation is likely to cause loss of input that is difficult to diagnose and understand.

--------------------------------------
Type Limits for Call-Ins and Call-Outs
--------------------------------------

Depending on the direction (I, O, or IO) of a particular type, both call-ins and call-outs may transfer a value in two directions as follows:

.. parsed-literal::
   Call-out: GT.M -> C -> GT.M       Call-in:     C -> GT.M -> C
               |     |     |                      |     |     |
               '-----'-----'                      '-----'-----'
                  1     2                            2     1

In the following table, the YottaDB/GT.M->C limit applies to 1 and the C->YottaDB/GT.M limit applies to 2. In other words, YottaDB/GT.M->C applies to I direction for call-outs and O direction for call-ins and C->YottaDB/GT.M applies to I direction for call-ins and O direction for call-outs.

+----------------------------------------------------------------------------------+-------------------------------------------------------------------+----------------------------------------------------------------------+
|                                                                                  | YottaDB/GT.M->C                                                   | C->YottaDB/GT.M                                                      |
+==================================================================================+====================+==============================================+============================+=========================================+
| **Type**                                                                         | **Precision**      | **Range**                                    | **Precision**              | **Range**                               |
+----------------------------------------------------------------------------------+--------------------+----------------------------------------------+----------------------------+-----------------------------------------+
| gtm_int_t, gtm_int_t *                                                           | Full               | [-2^31+1, 2^31-1]                            | Full                       | [-2^31, 2^31-1]                         |
+----------------------------------------------------------------------------------+--------------------+----------------------------------------------+----------------------------+-----------------------------------------+
| gtm_uint_t, gtm_uint_t *                                                         | Full               | [0, 2^32-1]                                  | Full                       | [0, 2^32-1]                             |
+----------------------------------------------------------------------------------+--------------------+----------------------------------------------+----------------------------+-----------------------------------------+
| gtm_long_t, gtm_long_t * (64-bit)                                                | 18 digits          | [-2^63+1, 2^63-1]                            | 18 digits                  | [-2^63, 2^63-1]                         |
+----------------------------------------------------------------------------------+--------------------+----------------------------------------------+----------------------------+-----------------------------------------+
| gtm_long_t, gtm_long_t * (32-bit)                                                | Full               | [-2^31+1, 2^31-1]                            | Full                       | [-2^31, 2^31-1]                         |
+----------------------------------------------------------------------------------+--------------------+----------------------------------------------+----------------------------+-----------------------------------------+
| gtm_ulong_t, gtm_ulong_t * (64-bit)                                              | 18 digits          | [0, 2^64-1]                                  | 18 digits                  | [0, 2^64-1]                             |
+----------------------------------------------------------------------------------+--------------------+----------------------------------------------+----------------------------+-----------------------------------------+
| gtm_ulong_t, gtm_ulong_t * (32-bit)                                              | Full               | [0, 2^32-1]                                  | Full                       | [0, 2^32-1]                             |
+----------------------------------------------------------------------------------+--------------------+----------------------------------------------+----------------------------+-----------------------------------------+
| gtm_float_t, gtm_float_t *                                                       | 6-9 digits         | [1E-43, 3.4028235E38]                        | 6 digits                   | [1E-43, 3.4028235E38]                   |
+----------------------------------------------------------------------------------+--------------------+----------------------------------------------+----------------------------+-----------------------------------------+
| gtm_double_t, gtm_double_t *                                                     | 15-17 digits       | [1E-43, 1E47]                                | 15 digits                  | [1E-43, 1E47]                           |
+----------------------------------------------------------------------------------+--------------------+----------------------------------------------+----------------------------+-----------------------------------------+
| gtm_char_t *                                                                     | N/A                | ["", 1MiB]                                   | N/A                        | ["", 1MiB]                              |
+----------------------------------------------------------------------------------+--------------------+----------------------------------------------+----------------------------+-----------------------------------------+
| gtm_char_t **                                                                    | N/A                | ["", 1MiB]                                   | N/A                        | ["", 1MiB]                              |
+----------------------------------------------------------------------------------+--------------------+----------------------------------------------+----------------------------+-----------------------------------------+
| gtm_string_t *                                                                   | N/A                | ["", 1MiB]                                   | N/A                        | ["", 1MiB]                              |
+----------------------------------------------------------------------------------+--------------------+----------------------------------------------+----------------------------+-----------------------------------------+

.. note::
   gtm_char_t ** is not supported for call-ins but they are included for IO and O direction usage with call-outs. For call-out use of gtm_char_t * and gtm_string_t \*, the specification in the interface definition for preallocation sets the range for IO and O, with a maximum of 1MiB.



