
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





