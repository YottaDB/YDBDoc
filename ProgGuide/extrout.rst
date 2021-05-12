.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2018-2021 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This source code contains the intellectual property     #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. index::
   Integrating External Routines

==================================
11. Integrating External Routines
==================================

.. contents::
   :depth: 5

----------------------
Introduction
----------------------

Application code written in M can call application code written in C (or which uses a C compatible call) and vice versa.

.. note::
   This C code shares the process address space with the YottaDB run-time library and M application code. Bugs in C code may result in difficult to diagnose failures to occur in places not obviously related to the cause of the failure.

------------------------
Access to non-M Routines
------------------------

In YottaDB, calls to C language routines may be made with the following syntax:

.. code-block:: none

   DO &[packagename.]name[^name][parameter-list]

or as an expression element,

.. code-block:: none

   $&[packagename.]name[^name][parameter-list]

Where packagename, like the name elements, is a valid M name. Because of the parsing conventions of M, the identifier between the ampersand (&) and the optional parameter-list has precisely constrained punctuation - a later section describes how to transform this into a more richly punctuated name, should that be appropriate for the called function. While the intent of the syntax is to permit the name^name to match an M labelref, there is no semantic implication to any use of the up-arrow (^). For more information on M names, labelrefs and parameter-lists, refer to `Chapter 5: “General Language Features of M” <./langfeat.html>`_.

Example:

.. code-block:: none

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

The method of creating a shareable library varies by the operating system.

On Linux x86:

Example:

.. code-block:: none

   % gcc -c -fPIC -I$ydb_dist increment.c decrement.c
   % gcc -o libcrement.so -shared increment.o decrement.o

--------------------------
Using External Calls
--------------------------

The functions in programs increment and decrement are now available to YottaDB through the shareable library libcrement.sl or libcrement.so, or though the DLL as libcrement.dll, depending on the specific platform. The suffix .sl is used throughout the following examples to represent .sl, .so, or .dll. Be sure to use the appropriate suffix for your platform.

YottaDB uses an "external call table" to map the typeless data of M into the typed data of C, and vice versa. The external call table has a first line containing the pathname of the shareable library file followed by one or more specification lines in the following format:

.. code-block:: none

   entryref: return-value routine-name (parameter, parameter, ... ) [: SIGSAFE]

The optional case-insensitive keyword SIGSAFE following the parameter list specifies that the external call does not create its own signal handlers. This allows YottaDB to avoid burdensome signal handler coordination for the external call. By default, YottaDB saves and restores signal setups for external calls.

entryref is an M entryref, return-value is ydb_long_t, ydb_status_t, or void, and parameters are in the format:

.. code-block:: none

   direction:type [num]

where [num] indicates a pre-allocation value explained later in this chapter.

Legal directions are I, O, or IO for input, output, or input/output, respectively.

The following table describes the legal types defined in the C header file $ydb_dist/libyottadb.h:

+++++++++++++
Type: Usage
+++++++++++++

Void: Specifies that the function does not return a value.

ydb_status_t : Type int. If the function returns zero (0), then the call was successful. If it returns a non-zero value, YottaDB will signal an error upon returning to M.

ydb_long_t : 32-bit signed integer on 32-bit platforms and 64-bit signed integer on 64-bit platforms.

ydb_ulong_t : 32-bit unsigned integer on 32-bit platforms and 64-bit signed integer on 64-bit platforms.

ydb_long_t* : For passing a pointer to long [integers].

ydb_float_t* : For passing a pointer to floating point numbers.

ydb_double_t* : Same as above, but double precision.

ydb_char_t*: For passing a "C" style string - null terminated.

ydb_char_t** : For passing a pointer to a "C" style string.

ydb_string_t* : For passing a structure in the form {int length;char \*address}. Useful for moving blocks of memory to or from YottaDB.

ydb_pointertofunc_t : For passing callback function pointers. For details see :ref:`callback-mech`.

.. note::
   If an external call's function argument is defined in the external call table, YottaDB allows invoking that function without specifying a value of the argument. All non-trailing and output-only arguments which do not specify a value translate to the following default values in C:

   * All numeric types: 0
   * ydb_char_t * and ydb_char_t \*\*: Empty string
   * ydb_string_t \*: A structure with 'length' field matching the preallocation size and 'address' field being a NULL pointer.

In the mathpak package example, the following invocation translate inval to the default value, that is, 0.

.. code-block:: bash

   YDB>do &mathpak.increment(,.outval)

If an external call's function argument is defined in the external call table and that function is invoked without specifying the argument, ensure that the external call function appropriately handles the missing argument. As a good programming practice, always ensure that count of arguments defined in the external call table matches the function invocation.

libyottadb.h also includes definitions for the following entry points exported from libyottadb:

.. code-block:: C

   void ydb_hiber_start(ydb_uint_t mssleep);
   void ydb_hiber_start_wait_any(ydb_uint_t mssleep)
   void ydb_start_timer(ydb_tid_t tid, ydb_int_t time_to_expir, void (*handler)(), ydb_int_t hdata_len, void *hdata);
   void ydb_cancel_timer(ydb_tid_t tid);

where:

* mssleep - milliseconds to sleep
* tid - unique timer id value
* time_to_expir - milliseconds until timer drives given handler
* handler - function pointer to handler to be driven
* hdata_len - 0 or length of data to pass to handler as a parameter
* hdata - NULL or address of data to pass to handler as a parameter

ydb_hiber_start() always sleeps until the time expires; ydb_hiber_start_wait_any() sleeps until the time expires or an interrupt by any signal (including another timer). ydb_start_timer() starts a timer but returns immediately (no sleeping) and drives the given handler when time expires unless the timer is canceled.

.. note::
   YottaDB continues to support xc_* equivalent types of ydb_* for upward compatibility. gtmxc_types.h explicitly marks the xc_* equivalent types as deprecated.

* Parameter-types that interface YottaDB with non-M code using C calling conventions must match the data-types on their target platforms. Note that most addresses on 64-bit platforms are 8 bytes long and require 8 byte alignment in structures whereas all addresses on 32-bit platforms are 4 bytes long and require 4-byte alignment in structures.
* Though strings with embedded NULL characters are sent as input to external routines, embedded NULL characters in output (or return value) strings of type ydb_char_t may cause string truncation because they are treated as terminators.
* If your interface uses ydb_long_t or ydb_ulong_t types but your interface code uses int or signed int types, failure to revise the types so they match on a 64-bit platform will cause the code to fail in unpleasant, potentially dangerous and hard to diagnose ways.

The first parameter of each called routine is an int (for example, int argc in decrement.c and increment.c) that specifies the number of parameters passed. This parameter is implicit and only appears in the called routine. It does not appear in the call table specification, or in the M invocation. If there are no explicit parameters, the call table specification will have a zero (0) value because this value does not include itself in the count. If there are fewer actual parameters than formal parameters, the call is determined from the parameters specified by the values supplied by the M program. The remaining parameters are undefined. If there are more actual parameters than formal parameters, YottaDB reports an error.

There may be only a single occurrence of the type ydb_status_t for each entryref.

++++++++++++++++++++++++++++++++++++
Pre-allocation of Output Parameters
++++++++++++++++++++++++++++++++++++

The definition of parameters passed by reference with direction output can include specification of a pre-allocation value. This is the number of units of memory that the user wants YottaDB to allocate before passing the parameter to the external routine. For example, in the case of type ydb_char_t \*, the pre-allocation value would be the number of bytes to be allocated before the call to the external routine.

Specification of a pre-allocation value should follow these rules:

* Pre-allocation is an unsigned integer value specifying the number of bytes to be allocated on the system heap with a pointer passed into the external call.
* Pre-allocating on a type with a direction of input or input/output results in a YottaDB error.
* Pre-allocation is meaningful only on types ydb_char_t * and ydb_string_t \*. On all other types the pre-allocation value specified will be ignored and the parameter will be allocated a default value for that type. With ydb_string_t * arguments make sure to set the 'length' field appropriately before returning control to YottaDB. On return from the external call, YottaDB uses the value in the length field as the length of the returned value, in bytes.
* If the user does not specify any value, then the default pre-allocation value would be assigned to the parameter.
* Specification of pre-allocation for "scalar" types (parameters which are passed by value) is an error.

.. note::
   Pre-allocation is optional for all output-only parameters except ydb_string_t * and ydb_char_t \*. Pre-allocation yields better management of memory for the external call. When an external call exceeds its specified preallocation (ydb_string_t * or ydb_char_t * output), YottaDB produces the EXCEEDSPREALLOC error. In the case that the user allocates space for the character pointer inside a ydb_string_t * type output parameter, a length field longer than the specified preallocated size for the output parameter does not cause an EXCEEDSPREALLOC error.

 .. _callback-mech:

+++++++++++++++++++++++++++++
Callback Mechanism
+++++++++++++++++++++++++++++

YottaDB exposes certain functions that are internal to the YottaDB runtime library for the external calls via a callback mechanism. While making an external call, YottaDB populates and exposes a table of function pointers containing addresses to call-back functions.

+----------+---------------------+--------------------+--------------------+-------------------------------------------------------------------------------------------+
| Index    | Function            | Argument           | Type               | Description                                                                               |
+==========+=====================+====================+====================+===========================================================================================+
| 0        | hiber_start         |                    |                    | sleep for a specified time                                                                |
+----------+---------------------+--------------------+--------------------+-------------------------------------------------------------------------------------------+
|          |                     | slp_time           | integer            | milliseconds to sleep                                                                     |
+----------+---------------------+--------------------+--------------------+-------------------------------------------------------------------------------------------+
| 1        | hiber_start_wait_any|                    |                    | sleep for a specified time or until any interrupt, whichever comes first                  |
+----------+---------------------+--------------------+--------------------+-------------------------------------------------------------------------------------------+
|          |                     | slp_time           | integer            | milliseconds to sleep                                                                     |
+----------+---------------------+--------------------+--------------------+-------------------------------------------------------------------------------------------+
| 2        | start_timer         |                    |                    | start a timer and invoke a handler function when the timer expires                        |
+----------+---------------------+--------------------+--------------------+-------------------------------------------------------------------------------------------+
|          |                     | tid                | integer            | unique user specified identifier for this timer                                           |
+----------+---------------------+--------------------+--------------------+-------------------------------------------------------------------------------------------+
|          |                     | time_to_expire     | integer            | milliseconds before handler is invoked                                                    |
+----------+---------------------+--------------------+--------------------+-------------------------------------------------------------------------------------------+
|          |                     | handler            | pointer to function| specifies the entry of the handler function to invoke                                     |
+----------+---------------------+--------------------+--------------------+-------------------------------------------------------------------------------------------+
|          |                     | hlen               | integer            | length of data to be passed via the hdata argument                                        |
+----------+---------------------+--------------------+--------------------+-------------------------------------------------------------------------------------------+
|          |                     | hdata              | pointer to char    | data (if any) to pass to the handler function                                             |
+----------+---------------------+--------------------+--------------------+-------------------------------------------------------------------------------------------+
| 3        | cancel_timer        |                    |                    | stop a timer previously started with start_timer(), if it has not yet expired             |
+----------+---------------------+--------------------+--------------------+-------------------------------------------------------------------------------------------+
|          |                     | tid                | integer            | unique user specified identifier of the timer to cancel                                   |
+----------+---------------------+--------------------+--------------------+-------------------------------------------------------------------------------------------+
| 4        | ydb_malloc          |                    |                    | allocates process memory from the heap                                                    |
+----------+---------------------+--------------------+--------------------+-------------------------------------------------------------------------------------------+
|          |                     | <return-value>     | pointer to void    | address of the allocated space                                                            |
+----------+---------------------+--------------------+--------------------+-------------------------------------------------------------------------------------------+
|          |                     | space needed       | 32-bit platforms:  | bytes of space to allocate. This has the same signature as the system malloc() call.      |
|          |                     |                    | 32-bit unsigned    |                                                                                           |
|          |                     |                    | integer            |                                                                                           |
|          |                     |                    |                    |                                                                                           |
|          |                     |                    | 64-bit platforms:  |                                                                                           |
|          |                     |                    | 64-bit unsigned    |                                                                                           |
|          |                     |                    | integer            |                                                                                           |
+----------+---------------------+--------------------+--------------------+-------------------------------------------------------------------------------------------+
| 5        | ydb_free            |                    |                    | return memory previously allocated with ydb_malloc()                                      |
+----------+---------------------+--------------------+--------------------+-------------------------------------------------------------------------------------------+
|          |                     | free_address       | pointer to void    | address of the previously allocated space                                                 |
+----------+---------------------+--------------------+--------------------+-------------------------------------------------------------------------------------------+

The external routine can access and invoke a call-back function in any of the following mechanisms:

* While making an external call, YottaDB sets the environment variable GTM_CALLIN_START to point to a string containing the start address (decimal integer value) of the table described above. The external routine needs to read this environment variable, convert the string into an integer value and should index into the appropriate entry to call the appropriate YottaDB function.
* YottaDB also provides an input-only parameter type ydb_pointertofunc_t that can be used to obtain call-back function pointers via parameters in the external routine. If a parameter is specified as I:ydb_pointertofunc_t and if a numeric value (between 0-5) is passed for this parameter in M, YottaDB interprets this value as the index into the callback table and passes the appropriate callback function pointer to the external routine.

.. note::
   YottaDB strongly discourages the use of signals, especially SIGALARM, in user written C functions. YottaDB assumes that it has complete control over any signals that occur and depends on that behavior for recovery if anything should go wrong. The use of exposed timer APIs should be considered for timer needs.

++++++++++++++++++++++++++++++++++++
Limitations on the External Program
++++++++++++++++++++++++++++++++++++

Since both YottaDB runtime environment and the external C functions execute in the same process space, the following restrictions apply to the external functions:

* YottaDB is designed to use signals and has signal handlers that must function for YottaDB to operate properly. The timer related call-backs should be used in place of any library or system call which uses SIGALRM such as sleep(). Use of signals by external call code may cause YottaDB to fail.
* Use of the YottaDB provided malloc and free, creates an integrated heap management system, which has a number of debugging tools. YottaDB recommends the usage of ydb_malloc/ydb_free in the external functions that provides better debugging capability in case memory management problems occur with external calls.
* Use of exit system call in external functions is strongly discouraged. Since YottaDB uses exit handlers to properly shutdown runtime environment and any active resources, the system call _exit should never be used in external functions.
* YottaDB uses timer signals so often that the likelihood of a system call being interrupted is high. So, all system calls in the external program can return EINTR if interrupted by a signal.
* Handler functions invoked with start_timer must not invoke services that are identified by the Operating System documentation as unsafe for signal handlers (or not identified as safe) - consult the system documentation or man pages for this information. Such services cause non-deterministic failures when they are interrupted by a function that then attempts to call them, wrongly assuming they are re-entrant.

The ydb_stdout_stderr_adjust() function checks whether stdout (file descriptor 1) and stderr (file descriptor 2) are the same file. If they are the same file, the function routes writes to stdout instead of stderr. This ensures that output appears in the order in which it was written. Otherwise, owing to IO buffering, output can appear in an order different from that in which it was written. Application code that mixes C and M code, and explicitly redirects stdout or stderr should call this function as soon as possible after the redirection. Refer to the function definition in the `Multi-Language Programmer's Guide <../MultiLangProgGuide/cprogram.html#ydb-stdout-stderr-adjust-adjustt-fn>`_.

++++++++++++++++++++++++++++++++++++++++
Examples of Using External Calls
++++++++++++++++++++++++++++++++++++++++

.. code-block:: C

   foo: void bar (I:ydb_float_t*, O:ydb_float_t*)

There is one external call table for each package. The environment variable "ydb_xc" must name the external call table file for the default package. External call table files for packages other than the default must be identified by environment variables of the form "ydb_xc_name".

The first of the external call tables is the location of the shareable library. The location can include environment variable names.

Example:

.. code-block:: none

   % echo $ydb_xc_mathpak
   /user/joe/mathpak.xc
   % echo lib /usr/
   % cat mathpak.xc
   $lib/mathpak.sl
   exp: ydb_status_t xexp(I:ydb_float_t*, O:ydb_float_t*)
   % cat exp.c
   ...
   int xexp(count, invar, outvar)
   int count;
   float *invar;
   float *outvar;
   {
    ...
   }
   % ydb
   ...
   YDB>d &mathpak.exp(inval,.outval)
   YDB>

Example : For preallocation:

.. code-block:: none

   % echo $ydb_xc_extcall
   /usr/joe/extcall.xc
   % cat extcall.xc
   /usr/lib/extcall.sl
   prealloc: void ydb_pre_alloc_a(O:ydb_char_t *[12])
   % cat extcall.c
   #include <stdio.h>
   #include <string.h>
   #include "libyottadb.h"
   void ydb_pre_alloc_a (int count, char *arg_prealloca)
   {
    strcpy(arg_prealloca, "New Message");
    return;
   }

Example : for call-back mechanism

.. code-block:: none

   % echo $ydb_xc
   /usr/joe/callback.xc
   % cat /usr/joe/callback.xc
   $MYLIB/callback.sl
   init:     void   init_callbacks()
   tstslp:  void   tst_sleep(I:ydb_long_t)
   strtmr: void   start_timer(I:ydb_long_t, I:ydb_long_t)
   % cat /usr/joe/callback.c
   #include <stdio.h>
   #include <stdlib.h>

   #include "libyottadb.h"

   void **functable;
   void (*setup_timer)(int , int , void (*)() , int , char *);
   void (*cancel_timer)(int );
   void (*sleep_interrupted)(int );
   void (*sleep_uninterrupted)(int );
   void* (*malloc_fn)(int);
   void (*free_fn)(void*);

   void  init_callbacks (int count)
   {
      char *start_address;

      start_address = (char *)getenv("GTM_CALLIN_START");

      if (start_address == (char *)0)
       {
        fprintf(stderr,"GTM_CALLIN_START is not set\n");
        return;
       }
      functable = (void **)atoi(start_address);
      if (functable == (void **)0)
      {
       perror("atoi : ");
       fprintf(stderr,"addresses defined by GTM_CALLIN_START not a number\n");
       return;
      }
      sleep_uninterrupted = (void (*)(int )) functable[0];
      sleep_interrupted = (void (*)(int )) functable[1];
      setup_timer = (void (*)(int , int, void (*)(), int, char *)) functable[2];
      cancel_timer = (void (*)(int )) functable[3];

      malloc_fn = (void* (*)(int)) functable[4];
      free_fn = (void (*)(void*)) functable[5];

      return;
   }

   void  sleep (int count, int time)
   {
      (*sleep_uninterrupted)(time);
   }

   void timer_handler ()
   {
      fprintf(stderr,"Timer Handler called\n");
      /* Do something */
   }

   void  start_timer (int count, int time_to_int, int time_to_sleep)
   {
      (*setup_timer)((int )start_timer, time_to_int, timer_handler, 0, 0);
      return;
   }
   void* xmalloc (int count)
   {
     return (*malloc_fn)(count);
   }

   void  xfree(void* ptr)
   {
     (*free_fn)(ptr);
   }

Example:ydb_malloc/ydb_free callbacks using ydb_pointertofunc_t

.. code-block:: none

   % echo $ydb_xc
   /usr/joe/callback.xc
   % cat /usr/joe/callback.xc
   /usr/lib/callback.sl
   init: void init_callbacks(I:ydb_pointertofunc_t, I:ydb_pointertofunc_t)
   % ydb
   YDB> do &.init(4,5)
   YDB>
   % cat /usr/joe/callback.c
   #include <stdio.h>
   #include <stdlib.h>
   #include "libyottadb.h"
   void* (*malloc_fn)(int);
   void (*free_fn)(void*);
   void init_callbacks(int count, void* (*m)(int), void (*f)(void*))
   {
       malloc_fn = m;
       free_fn = f;
   }

.. _calls-ext-rt-call-ins:

-----------------------------------------
Calls from External Routines: Call-Ins
-----------------------------------------

Call-In is a framework supported by YottaDB that allows a C/C++ program to invoke an M routine within the same process context. YottaDB provides a well-defined Call-In interface packaged as a run-time shared library that can be linked into an external C/C++ program.

+++++++++++++++++++++++++++
Relevant Files for Call-Ins
+++++++++++++++++++++++++++

To facilitate Call-Ins to M routines, the YottaDB distribution directory ($ydb_dist) contains the following files:

* libyottadb.so - A shared library that implements the YottaDB run-time system, including the Call-In API. If Call-Ins are used from a standalone C/C++ program, this library needs to be explicitly linked into the program. See “Building Standalone Programs”, which describes the necessary linker options on each supported platforms.
* yottadb - The YottaDB startup program that dynamically links with libyottadb.so.
* libyottadb.h - A C-header file containing the declarations of Call-In API.

.. note::
   .so is the recognized shared library file extension on most UNIX platforms.

The following sections describe the files relevant to using Call-Ins.

~~~~~~~~~~~~~~
libyottadb.h
~~~~~~~~~~~~~~

The header file provides signatures of all Call-In interface functions and definitions of those valid data types that can be passed from C to M. YottaDB strongly recommends that these types be used instead of native types (int, char, float, and so on), to avoid possible mismatch problems during parameter passing.

libyottadb.h defines the following types that can be used in Call-Ins.

+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------+
| Type                  | Usage                                                                                                                                            |
+=======================+==================================================================================================================================================+
| void                  | Used to express that there is no function return value                                                                                           |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------+
| ydb_int_t             | ydb_int_t has 32-bit length on all platforms.                                                                                                    |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------+
| ydb_int64_t           | ydb_int64_t has 64-bit length on 64-bit platforms, and is unsupported on 32-bit platforms.                                                       |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------+
| ydb_uint_t            | ydb_uint_t has 32-bit length on all platforms                                                                                                    |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------+
| ydb_uint64_t          | ydb_uint64_t has 64-bit length on 64-bit platforms, and is unsupported on 32-bit platforms.                                                      |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------+
| ydb_long_t            | ydb_long_t has 32-bit length on 32-bit platforms and 64-bit length on 64-bit platforms. It is much the same as the C language long type.         |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------+
| ydb_ulong_t           | ydb_ulong_t is much the same as the C language unsigned long type.                                                                               |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------+
| ydb_float_t           | floating point number                                                                                                                            |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------+
| ydb_double_t          | Same as above but double precision.                                                                                                              |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------+
| ydb_long_t*           | Pointer to ydb_long_t. Good for returning integers.                                                                                              |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------+
| ydb_ulong_t*          | Pointer to ydb_ulong_t. Good for returning unsigned integers.                                                                                    |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------+

.. code-block:: C

   typedef struct {
       ydb_long_t length;
       ydb_char_t* address;
   } ydb_string_t;

The pointer types defined above are 32-bit addresses on all 32-bit platforms. For 64-bit platforms, ydb_string_t* is a 64-bit address.

libyottadb.h also provides an input-only parameter type ydb_pointertofunc_t that can be used to obtain call-back function pointers via parameters in the external routine. If a parameter is specified as I:ydb_pointertofunc_t and if a numeric value (between 0-5) is passed for this parameter in M, YottaDB interprets this value as the index into the callback table and passes the appropriate callback function pointer to the external routine.

.. note::
   YottaDB represents values that fit in 18 digits as numeric values, and values that require more than 18 digits as strings.

libyottadb.h also includes definitions for the following entry points exported from libyottadb:

.. code-block:: C

   void ydb_hiber_start(ydb_uint_t mssleep);
   void ydb_hiber_start_wait_any(ydb_uint_t mssleep)
   void ydb_start_timer(ydb_tid_t tid, ydb_int_t time_to_expir, void (*handler)(), ydb_int_t hdata_len, void *hdata);
   void ydb_cancel_timer(ydb_tid_t tid);

where:

* mssleep - milliseconds to sleep
* tid - unique timer id value
* time_to_expir - milliseconds until timer drives given handler
* handler - function pointer to handler to be driven
* hdata_len - 0 or length of data to pass to handler as a parameter
* hdata - NULL or address of data to pass to handler as a parameter

ydb_hiber_start() always sleeps until the time expires; ydb_hiber_start_wait_any() sleeps until the time expires or an interrupt by any signal (including another timer). ydb_start_timer() starts a timer but returns immediately (no sleeping) and drives the given handler when time expires unless the timer is canceled.

.. note::
   libyottadb.h continues to be upward compatible with gtmxc_types.h. gtmxc_types.h explicitly marks the xc_* equivalent types as deprecated.

ydb_int64_6 and ydb_uint64_t are supported on 64-bit platforms effective release `r1.30. <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_ and have no corresponding gtm_* type.

.. _call-in-table:

~~~~~~~~~~~~~~~
Call-In table
~~~~~~~~~~~~~~~

The Call-In table file is a text file that contains the signatures of all M label references that get called from C. In order to pass the typed C arguments to the type-less M formallist, the environment variable ydb_ci must be defined to point to the Call-In table file path. Each signature must be specified separately in a single line. YottaDB reads this file and interprets each line according to the following convention (specifications within box brackets "[]", are optional):

.. code-block:: none

   <c-call-name> : <ret-type> <label-ref> ([<direction>:<param-type>,...])

where,

<label-ref>: is the entry point (that is a valid label reference) at which YottaDB starts executing the M routine being called-in

<c-call-name>: is a unique C identifier that is actually used within C to refer to <label-ref>

<direction>: is either I (input-only), O (output-only), or IO (input-output)

<ret-type>: is the return type of <label-ref>

.. note::
   Since the return type is considered as an output-only (O) parameter, the only types allowed are pointer types and void. Void cannot be specified as parameter.

<param-type>: is a valid parameter type. Empty parentheses must be specified if no argument is passed to <label-ref>. The number of parameters DOES NOT have to match the number of parameters in the M function. Any parameters that are not supplied will be undefined in M. For example, your call-in table can map to an M function/procedure that takes 8 paramters, but the call-in could have only 2 parameters in the call-in table. That means that parameters 3-8 will be undefined when the M function/procedure is called.

The <direction> indicates the type of operation that YottaDB performs on the parameter read-only (I), write-only (O), or read-write (IO). All O and IO parameters must be passed by reference, that is, as pointers since YottaDB writes to these locations. All pointers that are being passed to YottaDB must be pre-allocated. The following table details valid type specifications for each direction.

+-------------------+---------------------------------------------------------------------------------------------------------------------------------------------+
| Directions        | Allowed Parameter Types                                                                                                                     |
+===================+=============================================================================================================================================+
| I                 | ydb_long_t, ydb_ulong_t, ydb_float_t, ydb_double_t,_ydb_long_t*, ydb_ulong_t*, ydb_float_t*, ydb_double_t*,_ydb_char_t*, ydb_string_t*      |
+-------------------+---------------------------------------------------------------------------------------------------------------------------------------------+
| O/IO              | ydb_long_t*, ydb_ulong_t*, ydb_float_t*, ydb_double_t*,_ydb_char_t*, ydb_string_t*                                                          |
+-------------------+---------------------------------------------------------------------------------------------------------------------------------------------+

Call-In tables support comments effective release `r1.30. <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_ YottaDB ignores text from a double slash (//) on a line to the end of the line.

Here is an example of Call-In table (ydb_access.ci) for _ydbaccess.m (see :ref:`call-ydb-from-c-prog`):

.. code-block:: none

   ydbget    : void get^%ydbaccess( I:ydb_char_t*, O:ydb_string_t*, O:ydb_char_t* )
   ydbkill   : void kill^%ydbaccess( I:ydb_char_t*, O:ydb_char_t* )
   ydblock   : void lock^%ydbaccess( I:ydb_char_t*, O:ydb_char_t* )
   ydborder  : void order^%ydbaccess( I:ydb_char_t*, O:ydb_string_t*, O:ydb_char_t* )
   ydbquery  : void query^%ydbaccess( I:ydb_char_t*, O:ydb_string_t*, O:ydb_char_t* )
   ydbset    : void set^%ydbaccess( I:ydb_char_t*, I:ydb_string_t*, O:ydb_char_t*)
   ydbxecute : void xecute^%ydbaccess( I:ydb_char_t*, O:ydb_char_t* )

.. _call-in-intf:

++++++++++++++++++++++++
Call-In Interface
++++++++++++++++++++++++

This section is further broken down into 6 subsections for an easy understanding of the Call-In interface. The section is concluded with an elaborate example.

~~~~~~~~~~~~~~~~~~~~
Initialize YottaDB
~~~~~~~~~~~~~~~~~~~~

.. code-block:: C

   ydb_status_t ydb_init(void);

If the base program is not an M routine but a standalone C program, ydb_init() must be called (before calling any YottaDB functions), to initialize the YottaDB run-time system.

ydb_init() returns zero (0) on success. On failure, it returns the YottaDB error status code whose message can be read into a buffer by immediately calling ydb_zstatus(). Duplicate invocations of ydb_init() are ignored by YottaDB.

If Call-Ins are used from an external call function (that is, a C function that has itself been called from M code), ydb_init() is not needed, because YottaDB is initialized before the External Call. All ydb_init() calls from External Calls functions are ignored by YottaDB.

~~~~~~~~~~~~~~~~~~~~~~~~~~
Call an M Routine from C
~~~~~~~~~~~~~~~~~~~~~~~~~~

YottaDB provides 4 interfaces for calling a M routine from C. These are:

* ydb_ci
* ydb_ci_t
* ydb_cip
* ydb_cip_t

ydb_cip  and ydb_cip_t offer better performance on calls after the first one.

While ydb_ci() and ydb_cip() are for single threaded applications, ydb_ci_t() and ydb_cip_t() are for multi-threaded applications that call M routines. See the `Threads <../MultiLangProgGuide/programmingnotes.html#threads>`_ section in the Multi-Language Programmer's Guide for details.

.. _ydb-ci-intf:

^^^^^^^^
ydb_ci
^^^^^^^^

.. code-block:: C

   ydb_status_t ydb_ci(const ydb_char_t* c_call_name, ...);

The variable argument function ydb_ci() is the interface that actually invokes a specified M routine and returns the results via parameters. The ydb_ci() call must be in the following format:

.. code-block:: C

   status = ydb_ci(<c_call_name> [, ret_val] [, arg1] ...);

First argument: c_call_name, a null-terminated C character string indicating the alias name for the corresponding <lab-ref> entry in the Call-In table.

Second argument (only to be supplied if <ret-type> is not void): ret_val, a pre-allocated pointer through which YottaDB returns the value of QUIT argument from the (extrinsic) M routine. ret_val must be the same type as specified for <ret-type> in the Call-In table entry.

List of arguments to be passed to the M routine's formallist: the number of arguments and the type of each argument must match the number of parameters, and parameter types specified in the corresponding Call-In table entry. **Note that passing the same number of arguments as the number of arguments in the Call-in table can cause undefined behavior, as the remaining arguments are picked up from uninitialized memory locations in the C stack!** All pointer arguments must be pre-allocated. YottaDB assumes that any pointer, which is passed for O/IO-parameter points to valid write-able memory.

The status value returned by ydb_ci() indicates the YottaDB status code: zero (0) if successful, or a non-zero error code on failure. The error string corrsponding to the failure code can be read into a buffer by immediately calling ydb_zstatus(). For more details, see the :ref:`ydb-zstatus` section below.

.. _ydb-ci-t-intf:

^^^^^^^^^^
ydb_ci_t
^^^^^^^^^^

.. code-block:: C

   int ydb_ci_t(uint64_t tptoken,  ydb_buffer_t *errstr, const char *c_rtn_name, ...);

The function ydb_ci_t() is an interface for a multi-threaded application to invoke an M routine..

The ydb_ci_t() call must be in the following format:

.. code-block:: C

   status= ydb_ci_t( <tptoken>, <errstrptr>, <ci_rtn_name> [,ret_val] [,arg1]...);

First argument: tptoken, a unique transaction processing token that refers to the active transaction.

Second argument: ci_rtn_name, a null-terminated C character string indicating the alias name for the corresponding <lab-ref> entry in the Call-In table.

ydb_ci_t() works in the same way and returns the same values as ydb_ci().

.. _ydb-cip-intf:

^^^^^^^^^
ydb_cip
^^^^^^^^^

.. code-block:: C

   ydb_status_t ydb_cip(ci_name_descriptor *ci_info, ...);

The variable argument function ydb_cip() is the interface that invokes the specified M routine and returns the results via parameters.

ci_name_descriptor has the following structure:

.. code-block:: C

   typedef struct
   {
     ydb_string_t rtn_name;
     void* handle;
   } ci_name_descriptor;

rtn_name is a C character string indicating the corresponding <lab-ref> entry in the Call-In table.

The :code:`handle` is YottaDB private information that YottaDB expects to be initialized to NULL before the first :code:`ydb_cip()` call using this :code:`ci_name_descriptor` structure. YottaDB initializes this field in the first call-in and uses this cached information on future :code:`ydb_cip()` calls to avoid a lookup of the routine name (compared to a :code:`ydb_ci()` call where routine name lookup happens on all calls). This :code:`handle` must be provided unmodified to YottaDB on subsequent calls. If application code modifies it, it will corrupt the address space of the process, and potentially cause just about any bad behavior that it is possible for the process to cause, including but not limited to process death, database damage and security violations.

The ydb_cip() call must follow the following format:

.. code-block:: C

   status = ydb_cip(<ci_name_descriptor> [, ret_val] [, arg1] ...);

First argument: ci_name_descriptor, as described above, within which rtn_name indicates the alias name for the corresponding <lab-ref> entry in the Call-In table.

Second argument (only to be supplied if <ret-type> is not void): ret_val, a pre-allocated pointer through which YottaDB returns the value of QUIT argument from the (extrinsic) M routine. ret_val must be the same type as specified for <ret-type> in the Call-In table entry.

List of arguments to be passed to the M routine's formallist: the number of arguments and the type of each argument must match the number of parameters, and parameter types specified in the corresponding Call-In table entry. **Note that passing the same number of arguments as the number of arguments in the Call-in table can cause undefined behavior, as the remaining arguments are picked up from uninitialized memory locations in the C stack!** All pointer arguments must be pre-allocated. YottaDB assumes that any pointer, which is passed for O/IO-parameter points to valid write-able memory.

The status value returned by ydb_cip() indicates the YottaDB status code: zero (0) if successful, or a non-zero error code on failure. The error message corrsponding to the failure code can be read into a buffer by immediately calling ydb_zstatus().

.. _ydb-cip-t-intf:

^^^^^^^^^^^
ydb_cip_t
^^^^^^^^^^^

.. code-block:: C

   int ydb_cip_t(uint64_t tptoken, ydb_buffer_t *errstr, const char *c_rtn_name, ...);

The function ydb_cip_t is an interface for a multi-threaded application to invoke an M routine.

The ydb_cip_t() call must follow the following format:

.. code-block:: C

   status = ydb_cip_t(<tptoken>, <errstrptr>, <ci_name_descriptor> [,ret_val] [,arg1] ...);

First argument: tptoken, a unique transaction processing token that refers to the active transaction.

ydb_cip_t() works in the same way and returns the same values as ydb_cip().

.. _call-ydb-from-c-prog:

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Example: Calling YottaDB from a C Program
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Here is a working example of a C program that uses call-ins to invoke YottaDB. The example is packaged in a zip file which contains a C program, a call-in table, and a YottaDB API. To run the example, download and follow the compiling and linking instructions in the comments of the C program.

+--------------------------------+----------------------------------------------------------------------------------------------+
| Example                        | Download Information                                                                         |
+================================+==============================================================================================+
| ydb_access.c (ydb_ci interface)| `ydbci_ydbaccess.zip <./ydbci_ydbaccess.zip>`_                                               |
+--------------------------------+----------------------------------------------------------------------------------------------+

~~~~~~~~~~~~~~~~~~~~~~
Print Error Messages
~~~~~~~~~~~~~~~~~~~~~~

.. _ydb-zstatus:

^^^^^^^^^^^^^
ydb_zstatus
^^^^^^^^^^^^^

.. code-block:: C

   int ydb_zstatus (ydb_char_t* msg_buffer, ydb_long_t buf_len);

This function returns the null-terminated $ZSTATUS message of the last failure via the buffer pointed by msg_buffer of size buf_len. The message is truncated to size buf_len if it does not fit into the buffer. ydb_zstatus() is useful if the external application needs the text message corresponding to the last YottaDB failure. A buffer of 2048 is sufficient to fit in any YottaDB message.

Effective release `r1.30. <https://gitlab.com/YottaDB/DB/YDB/-/tags/r1.30>`_, ydb_zstatus() has an :code:`int` return value with a value of YDB_ERR_INVSTRLEN if the buffer supplied is not large enough to hold the message and YDB_OK otherwise. ydb_zstatus() copies what can be copied to the buffer (including a null terminator byte) if the length is non-zero.

~~~~~~~~~~~~~~~~~~~
Exit from YottaDB
~~~~~~~~~~~~~~~~~~~

.. code-block:: C

   ydb_status_t  ydb_exit (void);

ydb_exit() can be used to shut down all databases and exit from the YottaDB environment that was created by a previous ydb_init().

Note that ydb_init() creates various YottaDB resources and keeps them open across multiple invocations of ydb_ci() until ydb_exit() is called to close all such resources. On successful exit, ydb_exit() returns zero (0), else it returns the $ZSTATUS error code.

ydb_exit() cannot be called from an external call function. YottaDB reports the error YDB-E-INVGTMEXIT if an external call function invokes ydb_exit(). Since the YottaDB run-time system must be operational even after the external call function returns, ydb_exit() is meant to be called only once during a process lifetime, and only from the base C/C++ program when YottaDB functions are no longer required by the program.

+++++++++++++++++++++++++++++
Building Standalone Programs
+++++++++++++++++++++++++++++

All external C functions that use call-ins should include the header file libyottadb.h that defines various types and provides signatures of call-in functions. To avoid potential size mismatches with the parameter types, YottaDB strongly recommends that gtm \*t types defined in libyottadb.h be used instead of the native types (int, float, char, etc).

To use call-ins from a standalone C program, it is necessary that the YottaDB runtime library (libyottadb.so) is explicitly linked into the program. If call-ins are used from an External Call function (which in turn was called from YottaDB through the existing external call mechanism), the External Call library does not need to be linked explicitly with libyottadb.so since YottaDB would have already loaded it.

The following section describes compiler and linker options that must be used for call-ins to work from a standalone C/C++ program.

* Compiler: -I$ydb_dist
* Linker: -L$ydb_dist -lyottadb -rpath $ydb_dist
* YottaDB advises that the C/C++ compiler front-end be used as the Linker to avoid specifying the system startup routines on the ld command. The compile can pass linker options to ld using -W option (for example, cc -W1, -R, $ydb_dist). For more details on these options, refer to the appropriate system's manual on the respective platforms.

++++++++++++++++++++++++++++++
Nested Call-Ins
++++++++++++++++++++++++++++++

Call-ins can be nested by making an external call function in-turn call back into YottaDB. Each ydb_ci() called from an External Call library creates a call-in base frame at $ZLEVEL 1 and executes the M routine at $ZLEVEL 2. The nested call-in stack unwinds automatically when the External Call function returns to YottaDB.

YottaDB currently allows up to 10 levels of nesting, if TP is not used, and less than 10 if YottaDB supports call-ins from a transaction (see “Rules to Follow in Call-Ins”). YottaDB reports the error YDB-E-CIMAXLEVELS when the nesting reaches its limit.

Following are the YottaDB commands, Intrinsic Special Variables, and functions whose behavior changes in the context of every new nested call-in environment.

ZGOTO 0 (zero) returns to the processing of the invoking non-M routine as does ZGOTO 1 (one) with no entryref, while ZGOTO 1:entryref replaces the originally invoked M routine and continues M execution.

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

1. External calls must not be fenced with TSTART/TCOMMIT if the external routine calls back into yottadb using the call-in mechanism.
2. The external application should never call exit() unless it has called ydb_exit() previously. YottaDB internally installs an exit handler that should never be bypassed.
3. The external application should never use any signals when YottaDB is active since YottaDB reserves them for its internal use. YottaDB provides the ability to handle SIGUSR1 within M (see “$ZINTerrupt” for more information). An interface is provided by YottaDB for timers.
4. YottaDB recommends the use of ydb_malloc() and ydb_free() for memory management by C code that executes in a YottaDB process space for enhanced performance and improved debugging. Always use ydb_malloc() to allocate returns for pointer types to prevent memory leaks.
5. YottaDB performs device input using the read() system service. UNIX documentation recommends against mixing this type of input with buffered input services in the fgets() family and ignoring this recommendation is likely to cause a loss of input that is difficult to diagnose and understand.

--------------------------------------
Type Limits for Call-Ins and Call-Outs
--------------------------------------

Depending on the direction (I, O, or IO) of a particular type, both call-ins and call-outs may transfer a value in two directions as follows:

.. code-block:: none

   Call-out: YottaDB -> C -> YottaDB       Call-in:     C -> YottaDB -> C
               |        |       |                        |      |       |
               '--------'-------'                        '------'-------'
                  1     2                                   2     1

In the following table, the YottaDB->C limit applies to 1 and the C->YottaDB limit applies to 2. In other words, YottaDB->C applies to I direction for call-outs and O direction for call-ins and C->YottaDB applies to I direction for call-ins and O direction for call-outs.

+----------------------------------------------------+---------------------------------------------------+----------------------------------------------------------------------+
|                                                    | YottaDB->C                                        | C->YottaDB                                                           |
+====================================================+====================+==============================+============================+=========================================+
| **Type**                                           | **Precision**      | **Range**                    | **Precision**              | **Range**                               |
+----------------------------------------------------+--------------------+------------------------------+----------------------------+-----------------------------------------+
| ydb_int_t, ydb_int_t *                             | Full               | [-2^31+1, 2^31-1]            | Full                       | [-2^31, 2^31-1]                         |
+----------------------------------------------------+--------------------+------------------------------+----------------------------+-----------------------------------------+
| ydb_uint_t, ydb_uint_t *                           | Full               | [0, 2^32-1]                  | Full                       | [0, 2^32-1]                             |
+----------------------------------------------------+--------------------+------------------------------+----------------------------+-----------------------------------------+
| ydb_long_t, ydb_long_t * (64-bit)                  | 18 digits          | [-2^63+1, 2^63-1]            | 18 digits                  | [-2^63, 2^63-1]                         |
+----------------------------------------------------+--------------------+------------------------------+----------------------------+-----------------------------------------+
| ydb_long_t, ydb_long_t * (32-bit)                  | Full               | [-2^31+1, 2^31-1]            | Full                       | [-2^31, 2^31-1]                         |
+----------------------------------------------------+--------------------+------------------------------+----------------------------+-----------------------------------------+
| ydb_ulong_t, ydb_ulong_t * (64-bit)                | 18 digits          | [0, 2^64-1]                  | 18 digits                  | [0, 2^64-1]                             |
+----------------------------------------------------+--------------------+------------------------------+----------------------------+-----------------------------------------+
| ydb_ulong_t, ydb_ulong_t * (32-bit)                | Full               | [0, 2^32-1]                  | Full                       | [0, 2^32-1]                             |
+----------------------------------------------------+--------------------+------------------------------+----------------------------+-----------------------------------------+
| ydb_float_t, ydb_float_t *                         | 6-9 digits         | [1E-43, 3.4028235E38]        | 6 digits                   | [1E-43, 3.4028235E38]                   |
+----------------------------------------------------+--------------------+------------------------------+----------------------------+-----------------------------------------+
| ydb_double_t, ydb_double_t *                       | 15-17 digits       | [1E-43, 1E47]                | 15 digits                  | [1E-43, 1E47]                           |
+----------------------------------------------------+--------------------+------------------------------+----------------------------+-----------------------------------------+
| ydb_char_t *                                       | N/A                | ["", 1MiB]                   | N/A                        | ["", 1MiB]                              |
+----------------------------------------------------+--------------------+------------------------------+----------------------------+-----------------------------------------+
| ydb_char_t **                                      | N/A                | ["", 1MiB]                   | N/A                        | ["", 1MiB]                              |
+----------------------------------------------------+--------------------+------------------------------+----------------------------+-----------------------------------------+
| ydb_string_t *                                     | N/A                | ["", 1MiB]                   | N/A                        | ["", 1MiB]                              |
+----------------------------------------------------+--------------------+------------------------------+----------------------------+-----------------------------------------+

.. note::
   ydb_char_t ** is not supported for call-ins but they are included for IO and O direction usage with call-outs. For call-out use of ydb_char_t \* and ydb_string_t \*, the specification in the interface definition for preallocation sets the range for IO and O, with a maximum of 1MiB.

.. note::
   Call-ins where the return value is a string check for buffer overflows (where possible) and return an error if the return area is not large enough. Note that for string parameters, use of the :code:`ydb_string_t` type is highly recommended as it enables checking for buffer overflows. A :code:`char *` type does not enable such checks and is best avoided.

