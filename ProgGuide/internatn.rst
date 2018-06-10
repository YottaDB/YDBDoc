
.. index::
   Internationalization

============================
12. Internationalization
============================

.. contents::
   :depth: 2

This chapter describes YottaDB facilities for applications using characters that are encoded in other than eight-bit bytes (octets). Before continuing with the use of UTF-8 features, you will need to ensure that your system has installed and configured the needed infrastructure for the languages you wish to support, including International Components for Unicode (ICU/libicu), UTF-8 locale(s), and terminal emulators with appropriate fonts. This chapter addresses the specific issues of defining alternative collation sequences, and defining unique patterns for use with the pattern match operator.

Alternative collation sequences (or an alternative ordering of strings) can be defined for global and local variable subscripts. They can be established for specified globals or for an entire database. The alternative sequences are defined by a series of routines in an executable file pointed to by an environment variable. As the collation sequence is implemented by a user-supplied program, virtually any collation policy may be implemented. Detailed information on establishing alternative collation sequences and defining the environment variable is provided in the “Collation Sequence Definitions” below.

M has defined pattern classes that serve as arguments to the pattern match operator. YottaDB supports user definition of additional pattern classes as well as redefinition of the standard pattern classes. Specific patterns are defined in a text file that is pointed to by an environment variable. Pattern classes may be re-defined dynamically. The details of defining these pattern classes and the environment variables are described in the section called “Matching Alternative Patterns”.

For some languages (such as Chinese), the ordering of strings according to Unicode code-points (character values) may or may not be the linguistically or culturally correct ordering. Supporting applications in such languages requires the development of collation modules - YottaDB natively supports M collation, but does not include pre-built collation modules for any specific natural language. Therefore, applications that use characters in Unicode may need to implement their own collation functions. For more information on developing a collation module for Unicode, refer to `“Implementing an Alternative Collation Sequence for Unicode” <https://docs.yottadb.com/ProgrammersGuide/internatn.html#implementing-an-alternative-collation-sequence-for-unicode>`_.

-----------------------------------
Collation Sequence Definitions
-----------------------------------

Normally, YottaDB orders data with numeric values first, followed by strings sequenced by ASCII values. To use an alternative collating sequence, the following items must be provided at YottaDB process initialization.

* A shared library containing the routines for each alternative collation sequence
* An environment variable of the form ydb_collate_n, specifying the shared library containing the routines for alternative collation sequence n.

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Creating the Shared Library Holding the Alternative Sequencing Routines
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

A shared library for an alternative collation sequence must contain the following four routines:

* gtm_ac_xform_1: Transforms subscripts up to the maximum supported string length to the alternative collation sequence, or gtm_ac_xform: Transforms subscripts up to 32,767 bytes to the alternative collation sequence.
* gtm_ac_xback_1: Use with gtm_ac_xform_1 to transform the alternative collation keys back to the original subscript representation, or gtm_ac_xback: Use with gtm_ac_xform to transform the alternative collation keys back to the original subscript representation.
* gtm_ac_version: Returns a numeric version identifier for the "currently active" set of collation routines.
* gtm_ac_verify: Returns the success (odd) or failure (even) in matching a collation sequence with a given version number.

YottaDB searches the shared library for the gtm_ac_xform_1 and gtm_ac_xback_1 before searching for the gtm_ac_xform and gtm_ac_xback routines. If the shared library contains gtm_ac_xform_1, YottaDB ignores gtm_ac_xform even if it is present. If YottaDB finds gtm_ac_xform_1 but does not find gtm_ac_xback_1, it reports a YDB-E-COLLATIONUNDEF error with an additional mismatch warning YDB-E-COLLFNMISSING.

If the application does not use strings longer than 32,767 bytes, the alternative collation library need not contain the gtm_ac_xform_1 and gtm_ac_xback_1 routines. On the other hand, if the application passes strings greater than 32,767 bytes (but less than the maximum support string length) and does not provide gtm_xc_xform_1 and gtm_xc_xback_1, YottaDB issues the run-time error YDB-E-COLLARGLONG.

Note that database key sizes are much more restricted by YottaDB than local key sizes, and may be restricted further by user configuration.

+++++++++++++++++++++++++++++++++++
Defining the Environment Variable
+++++++++++++++++++++++++++++++++++

YottaDB locates the alternative collation sequences through the environment variable ydb_collate_n where n is an integer from 1 to 255 that identifies the collation sequence, and pathname identifies the shared library containing the routines for that collation sequence, for example:

.. parsed-literal::
   $ ydb_collate_1=/opt/yottadb/collation
   $ export ydb_collate_1

Multiple alternative collation sequence definitions can co-exist.

**Considerations in Establishing Alternative Collations**

Alternative collation sequences for a global must be set when the global contains no data. When the global is defined, the collation sequence is stored in the global. This ensures the future integrity of the global's collation. If it becomes necessary to change the collation sequence of a global containing data, you must copy the data to a temporary repository, delete the global, modify the variable's collation sequence by reinitializing the global either in a region that has the desired collation or with %GBLDEF, and restore the data from the temporary repository.

Be careful when creating the transformation and inverse transformation routines. The transformation routine must unambiguously and reliably encode every possible input value. The inverse routine must faithfully return the original value in every case. Errors in these routines can produce delayed symptoms that could be hard to debug. These routines may not be written in M.

+++++++++++++++++++++++++++++++++++++++++++++
Defining a Default Database Collation Method
+++++++++++++++++++++++++++++++++++++++++++++

YottaDB lets you define an alternative collation sequence as the default when creating a new database. Subsequently, this default is applied when each new global is created.

This default collation sequence is set as a GDE qualifier for the ADD, CHANGE, and TEMPLATE commands using the following example with CHANGE:

.. parsed-literal::
   GDE>CHANGE -REGION DEFAULT -COLLATION_DEFAULT=<0-255>

This qualifier always applies to regions, and takes effect when a database is created with MUPIP CREATE. The output of GDE SHOW displays this value, and DSE DUMP -FILEHEADER also includes this information. In the absence of an alternative default collations sequence, the default used is 0, or ASCII.

The value cannot be changed once a database file is created, and will be in effect for the life of the database file. The same restriction applies to the version of the collation sequence. The version of a collation sequence implementation is also stored in the database fileheader and cannot be modified except by recreating the file.

If the code of the collation sequence changes, making it incompatible with the collation sequence in use when the database was created, use the following procedure to ensure the continued validity of the database. MUPIP EXTRACT the database using the older compatible collation routines, then recreate and MUPIP LOAD using the newer collation routines.

+++++++++++++++++++++++++++++++++++++++++++++
Establishing A Local Collation Sequence
+++++++++++++++++++++++++++++++++++++++++++++

All subscripted local variables for a process must use the same collation sequence. The collation sequence used by local variables can be established as a default or in the current process. The local collation sequence can only be changed when a process has no subscripted local variables defined.

To establish a default local collation sequence provide a numeric value to the environment variable ydb_local_collate to select one of the collation tables, for example:

.. parsed-literal::
   $ ydb_local_collate=n
   $ export ydb_local_collate

where n is the number of a collation sequence that matches a valid collation number defined by an environment variable in the form ydb_collate_n.

An active process can use the %LCLCOL utility to define the collation sequence for subscripts of local variables. %LCLCOL has these extrinsic entry points:

set^%LCLCOL(n)changes the local collation to the type specified by n.

If the collation sequence is not available, the routine returns a false (0) and does not modify the local collation sequence.

Example:

.. parsed-literal::
   IF '$$set^%LCLCOL(3) D
   . Write "local collation sequence not changed",! Break

This piece of code illustrates $$set^LCLCOL used as an extrinsic. It would write an error message and BREAK if the local collation sequence was not set to 3.

set^%LCLCOL(n,ncol) determines the null collation type to be used with the collation type n. 

* If the truth value of ncol is FALSE(0), local variables use the YottaDB standard null collation.
* If the truth value of ncol is TRUE(1), local variables use the M standard null collation.

With set^%LCLCOL(,ncol), the null collation order can be changed while keeping the alternate collation order unchanged. If subscripted local variables exist, the null collation order cannot be changed. In this case, YottaDB issues YDB-E-COLLDATAEXISTS.

get^%LCLCOL returns the current local type.

Example:

.. parsed-literal::
   YDB>Write $$get^%LCLCOL
   0

This example uses $$get^%LCLCOL as an extrinsic that returns 0, indicating that the effective local collation sequence is the standard M collation sequence.

If set^%LCLCOL is not specified and ydb_local_collate is not defined, or is invalid, the process uses M standard collation. The following would be considered invalid values:

* A value less than 0
* A value greater than 255
* A legal collation sequence that is inaccessible to the process

Inaccessibility could be caused by a missing environment variable, a missing image, or security denial of access.

------------------------------------------
Creating the Alternate Collation Routines
------------------------------------------

Each alternative collation sequence requires a set of four user-created routines--gtm_ac_xform_1 (or gtm_ac_xform), gtm_ac_xback_1 (or gtm_ac_xback), gtm_ac_version, and gtm_ac_verify. The original and transformed strings are passed between YottaDB and the user-created routines using parameters of type gtm_descriptor or gtm32_descriptor. An "include file" gtm_descript.h, located in the YottaDB distribution directory, defines gtm_descriptor (used with gtm_ac_xform and gtm_ac_xback) as:

.. parsed-literal::
   typedef struct
   {
       short len;
       short type;
       void \*val;
    } gtm_descriptor;

.. note::
   On 64-bit UNIX platforms, gtm_descriptor may grow by up to eight (8) additional bytes as a result of compiler padding to meet platform alignment requirements.

gtm_descript.h defines gtm32_descriptor (used with gtm_xc_xform_1 and gtm_xc_xback_2) as:

.. parsed-literal::
   typedef struct
   {
       unsigned int len;
       unsigned int type;
       void \*val;
   } gtm32_descriptor;

where len is the length of the data, type is set to DSC_K_DTYPE_T (indicating that this is an M string), and val points to the text of the string.

The interface to each routine is described below.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Transformation Routine (gtm_ac_xform_1 or gtm_ac_xform)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++

gtm_ac_xform_1 or gtm_ac_xform routines transforms subscripts to the alternative collation sequence.

If the application uses subscripted lvns longer than 32,767 bytes (but less than 1,048,576 bytes), the alternative collation library must contain the gtm_ac_xform_1 and gtm_ac_xback_1 routines. Otherwise, the alternative collation library can contain gtm_ac_xform and gtm_ac_xback.

The syntax of this routine is:

.. parsed-literal::
   #include "gtm_descript.h"
   int gtm_ac_xform_1(gtm32_descriptor* in, int level, gtm32_descriptor* out, int* outlen);

**Input Arguments**

The input arguments for gtm_ac_xform are:

in: a gtm32_descriptor containing the string to be transformed.

level: an integer; this is not used currently, but is reserved for future facilities.

out: a gtm32_descriptor to be filled with the transformed key.

**Output Arguments**

return value: A long word status code.

out: A transformed subscript in the string buffer, passed by gtm32_descriptor.

outlen: A 32-bit signed integer, passed by reference, returning the actual length of the transformed key.

The syntax of gtm_ac_xform routine is:

.. parsed-literal::
   #include "gtm_descript.h"
   long gtm_ac_xform(gtm_descriptor \*in, int level, gtm_descriptor \*out, int \*outlen)

**Input Arguments**

The input arguments for gtm_ac_xform are:

in: a gtm_descriptor containing the string to be transformed.

level: an integer; this is not used currently, but is reserved for future facilities.

out: a gtm_descriptor to be filled with the transformed key.

**Output Arguments**

The output arguments for gtm_ac_xform are:

return value: a long result providing a status code; it indicates the success (zero) or failure (non-zero) of the transformation.

out: a gtm_descriptor containing the transformed key.

outlen: an unsigned long, passed by reference, giving the actual length of the output key.

Example: 

.. parsed-literal::
   #include "gtm_descript.h"
   #define MYAPP_SUBSC2LONG 12345678
   static unsigned char xform_table[256] =
   {
   0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
   16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
   32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
   48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
   64, 65, 67, 69, 71, 73, 75, 77, 79, 81, 83, 85, 87, 89, 91, 93,
   95, 97, 99,101,103,105,107,109,111,113,115,117,118,119,120,121,
   122, 66, 68, 70, 72, 74, 76, 78, 80, 82, 84, 86, 88, 90, 92, 94,
   96, 98,100,102,104,106,108,110,112,114,116,123,124,125,126,127,
   128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,
   144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,
   160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,
   176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,
   192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,
   208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,
   224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,
   240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255
   };
   long
   gtm_ac_xform (in, level, out, outlen)
   gtm_descriptor \*in;    /* the input string \*/
   int level;            /* the subscript level \*/
   gtm_descriptor \*out;    /* the output buffer \*/
   int \*outlen;        /* the length of the output string \*/
   {
   int n;
   unsigned char \*cp, \*cout;
   /* Ensure space in the output buffer for the string. \*/
   n = in->len;
   if (n > out->len)
   return MYAPP_SUBSC2LONG;
   /* There is space, copy the string, transforming, if necessary \*/
   cp = in->val;            /* Address of first byte of input string \*/
   cout = out->val;        /* Address of first byte of output buffer \*/
   while (n-- > 0)
   \*cout++ = xform_table[\*cp++];
   \*outlen = in->len;
   return 0;
   }

**Transformation Routine Characteristics**

The input and output values may contain <NUL> (hex code 00) characters.

The collation transformation routine may concatenate a sentinel, such as <NUL>, followed by the original subscript on the end of the transformed key. If the key length is not an issue, this permits the inverse transformation routine to simply retrieve the original subscript rather than calculating its value based on the transformed key.

If there are reasons not to append the entire original subscript, YottaDB allows you to concatenate a sentinel plus a predefined code so the original subscript can be easily retrieved by the inverse transformation routine, but still assures a reformatted key that is unique.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Inverse Transformation Routine (gtm_ac_xback or gtm_ac_xback_1)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

This routine returns altered keys to the original subscripts. The syntax of this routine is:

.. parsed-literal::
   #include "gtm_descript.h"
   long gtm_ac_xback(gtm_descriptor \*in, int level, gtm_descriptor \*out, int \*outlen)

The arguments of gtm_ac_xback are identical to those of gtm_ac_xform.

The syntax of gtm_ac_xback_1 is:

.. parsed-literal::
   #include "gtm_descript.h"
   long gtm_ac_xback_1 ( gtm32_descriptor \*src, int level, gtm32_descriptor \*dst, int \*dstlen)

The arguments of gtm_ac_xback_1 are identical to those of gtm_ac_xform_1.

Example:

.. parsed-literal::
   #include "gtm_descript.h"
   #define MYAPP_SUBSC2LONG 12345678
   static unsigned char inverse_table[256] =
   {
   0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
   16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
   32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
   48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
   64, 65, 97, 66, 98, 67, 99, 68,100, 69,101, 70,102, 71,103, 72,
   104, 73,105, 74,106, 75,107, 76,108, 77,109, 78,110, 79,111, 80,
   112, 81,113, 82,114, 83,115, 84,116, 85,117, 86,118, 87,119, 88,
   120, 89,121, 90,122, 91, 92, 93, 94, 95, 96,123,124,125,126,127,
   128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,
   144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,
   160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,
   176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,
   192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,
   208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,
   224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,
   240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255
   };
   long gtm_ac_xback (in, level, out, outlen)
   gtm_descriptor \*in;    /* the input string \*/
   int level;            /* the subscript level \*/
   gtm_descriptor \*out;    /* output buffer \*/
   int \*outlen;        /* the length of the output string \*/
   {
    int n;
    unsigned char \*cp, \*cout;
    /* Ensure space in the output buffer for the string. \*/
    n = in->len;
    if (n > out->len)
    return MYAPP_SUBSC2LONG;
    /* There is enough space, copy the string, transforming, if necessary \*/
    cp = in->val;            /* Address of first byte of input string \*/
    cout = out->val;        /* Address of first byte of output buffer \*/
    while (n-- > 0)
    \*cout++ = inverse_table[\*cp++];
    \*outlen = in->len;
    return 0;
   }

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version Control Routines (gtm_ac_version and gtm_ac_verify)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Two user-defined version control routines provide a safety mechanism to guard against a collation routine being used on the wrong global, or an attempt being made to modify a collation routine for an existing global. Either of these situations could cause incorrect collation or damage to subscripts.

When a global is assigned an alternative collation sequence, YottaDB invokes a user-supplied routine that returns a numeric version identifier for the set of collation routines, which was stored with the global. The first time a process accesses the global, YottaDB determines the assigned collation sequence, then invokes another user-supplied routine. The second routine matches the collation sequence and version identifier assigned to the global with those of the current set of collation routines.

When you write the code that matches the type and version, you can decide whether to modify the version identifier and whether to allow support of globals created using a previous version of the routine.

**Version Identifier Routine (gtm_ac_version)**

This routine returns an integer identifier between 0 and 255. This integer provides a mechanism to enforce compatibility as a collation sequence potentially evolves. When YottaDB first uses an alternate collation sequence for a database or global, it captures the version and if it finds the version has changed it at some later startup, it generates an error. The syntax is:

.. parsed-literal::
   int gtm_ac_version()

Example:

.. parsed-literal::
   int gtm_ac_version()
   { 
      return 1;
   }

**Verification Routine (gtm_ac_verify)**

This routine verifies that the type and version associated with a global are compatible with the active set of routines. Both the type and version are unsigned characters passed by value. The syntax is:

.. parsed-literal::
   #include "gtm_descript.h"
   int gtm_ac_verify(unsigned char type, unsigned char ver)

Example:

.. parsed-literal::
   Example:
   #include "gtm_descript.h"
   #define MYAPP_WRONGVERSION 20406080    /* User condition \*/
   gtm_ac_verify (type, ver)
        unsigned char type, ver;
   {
     if (type == 3)
      {
       if (ver > 2)        /* version checking may be more complex \*/
       {
        return 0;
       }
      }
     return MYAPP_WRONGVERSION;
   }

++++++++++++++++++++++++++++++
Using the %GBLDEF Utility
++++++++++++++++++++++++++++++

Use the %GBLDEF utility to get, set, or kill the collation sequence of a global variable mapped by the current global directory. %GBLDEF cannot modify the collation sequence for either a global containing data or a global whose subscripts span multiple regions. To change the collation sequence for a global variable that contains data, extract the data, KILL the variable, change the collation sequence, and reload the data. Use GDE to modify the collation sequence of a global variable that spans regions.

**Assigning the Collation Sequence**

To assign a collation sequence to an individual global use the extrinsic entry point:

.. parsed-literal::
   set^%GBLDEF(gname,nct,act)

where:

* The first argument, gname, is the name of the global. If the global name appears as a literal, it must be enclosed in quotation marks (" "). The must be a legal M variable name, including the leading caret (^).
* The second argument, nct, is an integer that determines whether numeric subscripts are treated as strings. The value is FALSE (0) if numeric subscripts are to collate before strings, as in standard M, and TRUE (1) if numeric subscripts are to be treated as strings (for example, where 10 collates before 9).
* The third argument, act, is an integer specifying the active collation sequence– from 0, standard M collation, to 255.
* If the global contains data, this function returns a FALSE (0) and does not modify the existing collation sequence definition.
* If the global's subscripts span multiple regions, the function returns a false (0). Use the global directory (GBLNAME object in GDE) to set collation characteristics for a global whose subscripts span multiple regions.
* Always execute this function outside of a TSTART/TCOMMIT fence. If $TLEVEL is non-zero, the function returns a false(0).

Example:

.. parsed-literal::
   YDB>kill ^G
   YDB>write $select($$set^%GBLDEF("^G",0,3):"ok",1:"failed")
   ok
   YDB>

This deletes the global variable ^G, then uses the \$\$set%GBLDEF as an extrinsic to set ^G to the collation sequence number 3 with numeric subscripts collating before strings. Using $$set%GBLDEF as an argument to $SELECT provides a return value as to whether or not the set was successful. $SELECT will return a "FAILED" message if the collation sequence requested is undefined.

**Examining Global Collation Characteristics**

To examine the collation characteristics currently assigned to a global use the extrinsic entry point:

.. parsed-literal::
   get^%GBLDEF(gname[,reg])

where gname specifies the global variable name. When gname spans multiple regions, reg specifies a region in the span.

This function returns the data associated with the global name as a comma delimited string having the following pieces:

* A truth-valued integer specifying FALSE (0) if numeric subscripts collate before strings, as in standard M, and TRUE (1) if numeric subscripts are handled as strings.
* An integer specifying the collation sequence.
* An integer specifying the version, or revision level, of the currently implemented collation sequence.

.. note::
   get^%GBLDEF(gname) returns global specific characteristics, which can differ from collation characteristics defined for the database file at MUPIP CREATE time from settings in the global directory. A "0" return from $$get^%gbldef(gname[,reg]) indicates that the global has no special characteristics and uses the region default collation, while a "0,0,0" return indicates that the global is explicitly defined to M collation. DSE DUMP -FILEHEADER command displays region collation whenever the collation is other than M standard collation.

Example:

.. parsed-literal::
   YDB>Write $$get^%GBLDEF("^G")
   1,3,1

This example returns the collation sequence information currently assigned to the global ^G.

**Deleting Global Collation Characteristics**

To delete the collation characteristics currently assigned to a global, use the extrinsic entry point:

.. parsed-literal::
   kill^%GBLDEF(gname)

* If the global contains data, the function returns a false (0) and does not modify the global.
* If the global's subscript span multiple regions, the function returns a false (0). Use the global directory (GBLNAME object in GDE) to set collation characteristics for a global whose subscripts span multiple regions.
* Always execute this function outside of a TSTART/TCOMMIT fence. If $TLEVEL is non-zero, the function returns a false (0).

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Example of Upper and Lower Case Alphabetic Collation Sequence
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

This example is to create an alternate collation sequence that collates upper and lower case alphabetic characters in such a way that the set of keys "du Pont," "Friendly," "le Blanc," and "Madrid" collates as:

* du Pont
* Friendly
* le Blanc
* Madrid

This is in contrast to the standard M collation that orders them as:

* Friendly
* Madrid
* du Pont
* le Blanc

.. note::
   No claim of copyright is made with respect to the code used in this example. Please do not use the code as-is in a production environment.

Please ensure that you have a correctly configured YottaDB installation, correctly configured environment variables, with appropriate directories and files.

Seasoned YottaDB users may want to download polish.c used in this example and proceed directly to the compiling and linking instructions. First time users may want to start from the beginning.

Create a new file called polish.c and put the following code:

.. parsed-literal::
   #include <stdio.h>
   #include "gtm_descript.h"
   #define COLLATION_TABLE_SIZE     256
   #define MYAPPS_SUBSC2LONG        12345678
   #define SUCCESS     0
   #define FAILURE     1                
   #define VERSION     0
   static unsigned char xform_table[COLLATION_TABLE_SIZE] =
             {
               0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
               16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
               32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
               48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
               64, 65, 67, 69, 71, 73, 75, 77, 79, 81, 83, 85, 87, 89, 91, 93,
               95, 97, 99,101,103,105,107,109,111,113,115,117,118,119,120,121,
               122, 66, 68, 70, 72, 74, 76, 78, 80, 82, 84, 86, 88, 90, 92, 94,
               96, 98,100,102,104,106,108,110,112,114,116,123,124,125,126,127,
               128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,
               144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,
               160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,
               176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,
               192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,
               208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,
               224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,
               240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255
             };
   static unsigned char inverse_table[COLLATION_TABLE_SIZE] =
             {
               0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
               16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
               32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
               48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
               64, 65, 97, 66, 98, 67, 99, 68,100, 69,101, 70,102, 71,103, 72,
               104, 73,105, 74,106, 75,107, 76,108, 77,109, 78,110, 79,111, 80,
               112, 81,113, 82,114, 83,115, 84,116, 85,117, 86,118, 87,119, 88,
               120, 89,121, 90,122, 91, 92, 93, 94, 95, 96,123,124,125,126,127,
               128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,
               144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,
               160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,
               176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,
               192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,
               208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,
               224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,
               240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255
             };

Elements in xform_table represent input order for transform. Elements in inverse_table represent reverse transform for x_form_table.

Add the following code for the gtm_ac_xform transformation routine:

.. parsed-literal::
   long gtm_ac_xform ( gtm_descriptor \*src, int level, gtm_descriptor \*dst, int \*dstlen)
         {
          int n;
          unsigned char  \*cp, \*cpout;
          #ifdef DEBUG
          char input[COLLATION_TABLE_SIZE], output[COLLATION_TABLE_SIZE];
          #endif
          n = src->len;
          if ( n > dst->len)
          return MYAPPS_SUBSC2LONG;
          cp  = (unsigned char \*)src->val;
          #ifdef DEBUG
          memcpy(input, cp, src->len);
          input[src->len] = '\0';
          #endif
          cpout = (unsigned char \*)dst->val;
          while ( n-- > 0 )
          \*cpout++ = xform_table[\*cp++];
          \*cpout = '\0';
          \*dstlen = src->len;
          #ifdef DEBUG
          memcpy(output, dst->val, dst->len);
          output[dst->len] = '\0';
          fprintf(stderr, "\nInput = \n");
          for (n = 0; n < \*dstlen; n++ ) fprintf(stderr," %d ",(int )input[n]);
          fprintf(stderr, "\nOutput = \n");
          for (n = 0; n < \*dstlen; n++ ) fprintf(stderr," %d ",(int )output[n]);
          #endif
          return SUCCESS;
         }

      
Add the following code for the gtm_ac_xback reverse transformation routine:

.. parsed-literal::
   long gtm_ac_xback ( gtm_descriptor \*src, int level, gtm_descriptor \*dst, int \*dstlen)
         {
          int n;
          unsigned char  \*cp, \*cpout;
          #ifdef DEBUG
          char input[256], output[256];
          #endif
          n = src->len;
          if ( n > dst->len)
          return MYAPPS_SUBSC2LONG;
          cp  = (unsigned char \*)src->val;
          cpout = (unsigned char \*)dst->val;
          while ( n-- > 0 )
          \*cpout++ = inverse_table[\*cp++];
          \*cpout = '\0';
          \*dstlen = src->len;
          #ifdef DEBUG
          memcpy(input, src->val, src->len);
          input[src->len] = '\';
          memcpy(output, dst->val, dst->len);
          output[dst->len] = '\0';
          fprintf(stderr, "Input = %s, Output = %s\n",input, output);
          #endif
          return SUCCESS;
         }

Add code for the version identifier routine (gtm_ac_version) or the verification routine (gtm_ac_verify):

.. parsed-literal::
   int gtm_ac_version ()
         {
           return VERSION;
         }
   int gtm_ac_verify (unsigned char type, unsigned char ver)
         {
           return !(ver == VERSION);
         }

Save and compile polish.c. On x86 GNU/Linux (64-bit Ubuntu 10.10), execute a command like the following:

.. parsed-literal::
   gcc -c polish.c -I$ydb_dist

.. note::
   The -I$ydb_dist option includes gtmxc_types.h.

Create a new shared library or add the above routines to an existing one. The following command adds these alternative sequence routines to a shared library called altcoll.so on x86 GNU/Linux (64-bit Ubuntu 10.10).

.. parsed-literal::
   gcc -o altcoll.so -shared polish.o

Set $ydb_collate_1 to point to the location of altcoll.so.

At the YDB> prompt execute the following command:

.. parsed-literal::
   YDB>Write $SELECT($$set^%GBLDEF("^G",0,1):"OK",1:"FAILED")
         OK

This deletes the global variable ^G, then sets ^G to the collation sequence number 1 with numeric subscripts collating before strings.

Assign the following value to ^G.

.. parsed-literal::
   YDB>Set ^G("du Pont")=1
   YDB>Set ^G("Friendly")=1
   YDB>Set ^G("le Blanc")=1
   YDB>Set ^G("Madrid")=1

See how the subscript of ^G order according to the alternative collation sequence:

.. parsed-literal::
   YDB>ZWRite ^G
   ^G("du Pont")=1
   ^G("Friendly")=1
   ^G("le Blanc")=1
   ^G("Madrid")=1

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Example of Collating Alphabets in Reverse Order using gtm_ac_xform_1 and gtm_ac_xback_1
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

This example creates an alternate collation sequence that collates alphabets in reverse order. This is in contrast to the standard M collation that collates alphabets in ascending order.

.. note::
   No claim of copyright is made with respect to the code used in this example. Please do not use the code as-is in a production environment.

Please ensure that you have a correctly configured YottaDB installation and correctly configured environment variables with appropriate directories and files.

Download col_reverse_32.c from `Github <https://github.com/YottaDB/YottaDBdoc/tree/master/ProgGuide/col_reverse_32.c>`_. It contains code for the transformation routine (gtm_ac_xform_1), reverse transformation routine (gtm_ac_xback_1) and the version control routines (gtm_ac_version and gtm_ac_verify).

Save and compile col_reverse_32.c. On x86 GNU/Linux (64-bit Ubuntu 10.10), execute a command like the following:

.. parsed-literal::
   gcc -c col_reverse_32.c -I$ydb_dist

.. note::
   The -I$ydb_dist option includes gtmxc_types.h.

Create a new shared library or add the routines to an existing one. The following command adds these alternative sequence routines to a shared library called altcoll.so on x86 GNU/Linux (64-bit Ubuntu 10.10).

.. parsed-literal::
   gcc -o revcol.so -shared col_reverse_32.o

Set the environment variable ydb_collate_2 to point to the location of revcol.so. To set the local variable collation to this alternative collation sequence, set the environment variable ydb_local_collate to 2.

At the prompt, execute the following command:

.. parsed-literal::
   YDB>Write $SELECT($$set^%GBLDEF("^E",0,2):"OK",1:"FAILED")
   OK

Assign the following values to ^E.

.. parsed-literal::
   YDB>Set ^E("du Pont")=1
   YDB>Set ^E("Friendly")=1
   YDB>Set ^E("le Blanc")=1
   YDB>Set ^E("Madrid")=1

Notice how the subscripts of ^E are sorted in reverse order:

.. parsed-literal::
   YDB>zwrite ^E
   ^G("le Blanc")=1
   ^G("du Pont")=1
   ^G("Madrid")=1
   ^G("Friendly")=1

----------------------------------------------------------------
Implementing an Alternative Collation Sequence for Unicode
----------------------------------------------------------------

By default, YottaDB sorts string subscripts in the default order of the Unicode numeric code-point ($ASCII()) values. Since this implied ordering may or may not be linguistically or culturally correct for a specific application, an implementation of an algorithm such as the Unicode Collation Algorithm (UCA) may be required. Note that the implementation of collation in YottaDB requires the implementation of two functions, f(x) and g(y). f(x) transforms each input sequence of bytes into an alternative sequence of bytes for storage. Within the YottaDB database engine, M nodes are retrieved according to the byte order in which they are stored. For each y that can be generated by f(x), g(y) is an inverse function that provides the original sequence of bytes; in other words, g(f(x)) must be equal to x for all x that the application processes. For example, for the People's Republic of China, it may be appropriate to convert from UTF-8 to Guojia Biaozhun (国家标准), the GB18030 standard, for example, using the libiconv library. The following requirements are important:

* **Unambiguous transformation routines**: The transform and its inverse must convert each input string to a unique sequence of bytes for storage, and convert each sequence of stored bytes back to the original string.
* **Collation sequence for all expected character sequences in subscripts**: YottaDB does not validate the subscript strings passed to/from the collation routines. If the application design allows illegal UTF-8 character sequences to be stored in the database, the collation functions must appropriately transform and inverse transform these as well.
* **Handle different string lengths for before and after transformation**: If the lengths of the input string and transformed string differ, and, for local variables, if the output buffer passed by YottaDB is not sufficient, follow the procedure described below:
  
   * Global Collation Routines: The transformed key must not exceed the lesser of the maximum key size configuration or 1019 bytes, the maximum GDS key size. YottaDB allocates a temporary buffer of size 1019 bytes in the output string descriptor (of type DSC_K_DTYPE_T) and passes it to the collation routine to return the transformed key.
   * Local Collation Routines: YottaDB allocates a temporary buffer in the output string descriptor based on the size of the input string. Both transformation and inverse transformation must check the buffer size, and if it is not sufficient, the transformation must allocate sufficient memory, set the output descriptor value (val field of the descriptor) to point to the new memory , and return the transformed key successfully. Since YottaDB copies the key from the output descriptor into its internal structures, it is important that the memory allocated remains available even after the collation routines return. Collation routines are typically called throughout the process lifetime, and therefore, YottaDB expects the collation libraries to define a large static buffer sufficient to hold all key sizes in the application. Alternatively, the collation transform can use a large heap buffer (allocated by the system malloc() or YottaDB gtm_malloc()). Application developers must choose the method best suited to their needs.

------------------------------------
Matching Alternative Patterns
------------------------------------

YottaDB allows the definition of unique patterns for use with the pattern match operator, in place of, or in addition to, the standard C, N, U, L, and P. You can redefine existing pattern codes (patcodes), or add new ones. These codes are defined in a specification file. The format is described in the next section.

+++++++++++++++++++++++++++++
Pattern Code Definition
+++++++++++++++++++++++++++++

This section explains the requirements for specifying alternative pattern codes. These specifications are created as a table in a file which YottaDB loads at run time.

Use the following keywords to construct your text file. Each keyword must:

* Appear as the first non-whitespace entry on a line.
* Be upper case.

The table names also must be uppercase. The patcodes are not case-sensitive.

PATSTART indicates the beginning of the definition text and must appear before the first table definition.

PATTABLE indicates the beginning of the table definition. The keyword PATTABLE is followed by a whitespace, then the table name. The text file can contain multiple PATTABLEs.

PATCODE indicates the beginning of a patcode definition. The keyword PATCODE is followed by a whitespace, then the patcode identifying character. On the next line enter a comma-delimited list of integer codes that satisfy the patcode. A PATCODE definition is always included in the most recently named PATTABLE. A PATTABLE can contain multiple PATCODEs.

PATEND indicates the end of the definition text; it must appear after the last table definition.

To continue the comma-delimited list on multiple lines, place a dash (-) at the end of each line that is not the last one in the sequence. To enter comments in the file, begin the line with a semi-colon (;).

The following example illustrates a possible patcode table called "NEWLANGUAGE". The example has definitions for patcodes "S," which would be a non-standard pattern character, and "L," which would substitute alternative definitions for the standard "L" (or lower case) pattern characters.

Example:

.. parsed-literal::
   PATSTART
     PATTABLE NEWLANGUAGE
     PATCODE S
         144,145,146,147,148,149,150
     PATCODE L
         230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255
   PATEND

Be mindful of the following items as you define your patcode table. 

* YottaDB loads a table name only once during the invocation of a process. Changes to a loaded table do not apply to running processes that have already referenced that table.
* The table name "M" is a reserved designation for standard M, which is included in the YottaDB run-time library.
* Standard patcodes A and E cannot be explicitly redefined. A is always the union of codes U and L; E always designates the set of all characters.
* The C pattern code you define is used by YottaDB to determine those characters which are to be treated as unprintable. All characters not defined as C are treated as printable.
* In UTF-8 mode, M standard patcodes (A,C,L,U,N,P,E) work with Unicode characters. Application developers can neither change their default classification nor define the non-standard patcodes ((B,D,F-K,M,O,Q-T,V-X) beyond the ASCII subset. This means that the pattern tables cannot contain characters with codes greater than the maximum ASCII code 127.

++++++++++++++++++++++
Pattern Code Selection
++++++++++++++++++++++

To establish a default patcode table for a database define the environment variable:

.. parsed-literal::
   $ ydb_pattern_file=pathname
   $ export ydb_pattern_file

where filename is the text file containing the patcode table definition, and 

.. parsed-literal::
   $ ydb_pattern_table=tablename
   $ export ydb_pattern_table

where tablename is the name of the patcode table within the file pointed to by ydb_pattern_file. 

.. note::
   YottaDB performs operations on literals at compile time and the pattern codes' settings may have an impact on such operations. Therefore, it is safest to always compile with the same pattern code settings as those used at runtime. If changes to pattern codes are required at run time, "hide" any patterns used on literal expressions from the compiler (which are uncommon) using XECUTE commands or indirection.

Within an active process, the patcode table is established using the M VIEW command and the %PATCODE utility. Before invoking the %PATCODE utility, you may use VIEW to load pattern definition files for YottaDB. The required keyword and value are:

.. parsed-literal::
   VIEW "PATLOAD":"pathname" 

This allows you to use the %PATCODE utility or the VIEW command to set current patcode table. The format of the VIEW command to set the patcode table is:

.. parsed-literal::
   VIEW "PATCODE":"tablename"

This is equivalent to set ^%PATCODE explained below.

%PATCODE has the following extrinsic entry points:

.. parsed-literal::
   set^%PATCODE(tn)

sets the current patcode table to the one having the name specified by tn, in the defined file specification.

Example:

.. parsed-literal::
   YDB>Write $$set^%PATCODE("NEWLANGUAGE")
   1

If there is no table with that name, the function returns a false (0) and does not modify the current patcode table.

.. parsed-literal::
   get^%PATCODE

returns the current patcode table name.

Example:

.. parsed-literal::
   YDB>Write $$get^%PATCODE
   NEWLANGUAGE 


