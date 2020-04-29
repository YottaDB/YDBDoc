/* This program is a driver to demonstrate a C main() program that calls in to YottaDB M code.
   No claim of copyright is made with respect to this code.

   Compile it and run it with:
     gcc -c ydb_access.c $(pkg-config --libs --cflags yottadb) -lyottadb
     gcc $(pkg-config --libs --cflags yottadb) ydb_access.o -o ydb_access -lyottadb
     ydb_ci=ydb_access.ci ./ydb_access

   The expected result of running this program is as follows (the platform, YottaDB version, and pid will vary)
     Washington, DC
     England
     United States
     ^Capital("United States")
     YottaDB r1.29 Linux x86_64
     MLG:1,MLT:0
     LOCK ^CIDemo(125366) LEVEL=1

   This program is only a demonstration.  Please ensure that you have a correctly
   configured YottaDB installation, correctly configured environment variables,
   with appropriate directories and files.

   This program is intended to call functionality implemented in M. For access to
   YottaDB global variables from C code, you should use the C API instead;
   see https://docs.yottadb.com/MultiLangProgGuide/cprogram.html

   DO NOT USE THIS CODE AS-IS IN A PRODUCTION ENVIRONMENT.

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>
#include "libyottadb.h"

/* YottaDB limits - you can use smaller numbers if your application is guaranteed to use smaller values
   but remember that this is a low-level programming interface that does not check for overflows
*/
#define maxcode 8192   /* maximum length of a line of code for the compiler / variable name */
#define maxmsg 2048    /* maximum length of a YottaDB message */
#define maxname 32     /* one more than the maximum length of a YottaDB name */
#define maxstr 1048576 /* maximum length of a value that YottaDB can return */

/* YottaDB call wrapper - if an error in call or untrappable error in YottaDB, print error on STDERR, clean up and exit
 */
#define CALLYDB(xyz) status = xyz ;		\
  if (0 != status ) {				\
    ydb_zstatus( msg, maxmsg );			\
    fprintf(stderr, "Failure of %s with error: %s\n", #xyz, msg );	\
    ydb_exit();					\
    return status ;				\
  }

int main() {

  ci_name_descriptor ydbget, ydbinit, ydbkill, ydblock, ydborder, ydbquery, ydbset, ydbxecute;
  char err[maxmsg], msg[maxmsg], value[maxstr], var[maxcode];
  ydb_status_t status;
  ydb_string_t p_value, ydbget_str, ydbinit_str, ydbkill_str, ydblock_str, ydborder_str, ydbquery_str, ydbset_str, ydbxecute_str;
  int i;

  /* Ensure required environment variables are set.
     PLEASE NOTE - This is only a demonstration program.  You will almost certainly want
     other environment variables to be defined in a production environment.
  */
  if ((NULL == getenv( "ydb_dist" )) | (NULL == getenv( "ydb_gbldir" )) |
      (NULL == getenv( "ydb_routines" )) | (NULL == getenv( "ydb_ci" ))) return 1;

  /* Initialization - string constants */
  char washington[] = "Washington, DC";
  char london[] = "London";

  /* Initialization - function descriptors for calling in to YottaDB */
  ydbget_str.address = "ydbget"; ydbget_str.length = sizeof("ydbget")-1;
  ydbget.rtn_name=ydbget_str; ydbget.handle = NULL;
  ydbinit_str.address = "ydbinit"; ydbinit_str.length = sizeof("ydbinit")-1;
  ydbinit.rtn_name=ydbinit_str; ydbinit.handle = NULL;
  ydbkill_str.address = "ydbkill"; ydbkill_str.length = sizeof("ydbkill")-1;
  ydbkill.rtn_name=ydbkill_str; ydbkill.handle = NULL;
  ydblock_str.address = "ydblock"; ydblock_str.length = sizeof("ydblock")-1;
  ydblock.rtn_name=ydblock_str; ydblock.handle = NULL;
  ydborder_str.address = "ydborder"; ydborder_str.length = sizeof("ydborder")-1;
  ydborder.rtn_name=ydborder_str; ydborder.handle = NULL;
  ydbquery_str.address = "ydbquery"; ydbquery_str.length = sizeof("ydbquery")-1;
  ydbquery.rtn_name=ydbquery_str; ydbquery.handle = NULL;
  ydbset_str.address = "ydbset"; ydbset_str.length = sizeof("ydbset")-1;
  ydbset.rtn_name=ydbset_str; ydbset.handle = NULL;
  ydbxecute_str.address = "ydbxecute"; ydbxecute_str.length = sizeof("ydbxecute")-1;
  ydbxecute.rtn_name=ydbxecute_str; ydbxecute.handle = NULL;

  err[0] = '\0'; /* Clear error message buffer by putting null terminator in first byte */

  /* Set a node - note that value can be an arbitrary blob, not just a null terminated string */
  p_value.address = (char *) &washington; p_value.length = strlen(washington);
  CALLYDB( ydb_cip( &ydbset, "^Capital(\"United States\")", &p_value, &err));
  if (0 != strlen( err )) fprintf( stdout, "ydbset1 failure: [%s]\n", err);

  /* Set another node */
  p_value.address = ( char *) &london ; p_value.length = strlen(london);
  CALLYDB( ydb_cip( &ydbset, "^Capital(\"England\")", &p_value, &err));
  if (0 != strlen( err )) fprintf( stdout, "ydbset2 failure: [%s]\n", err);

  /* Get the node first set & print it */
  p_value.address = ( char *) &value ; p_value.length = maxstr ;
  CALLYDB( ydb_cip( &ydbget, "^Capital(\"United States\")", &p_value, &err));
  if (0 != strlen( err )) fprintf( stdout, "ydbget1 failure: [%s]\n", err);
  else {
    for (i = 0 ; i < (int) p_value.length ; i++ ) fprintf( stdout, "%c", *(p_value.address+i));
    fprintf( stdout, "\n" );
  };

  /* Ordering through subscripts - first subscript */
  p_value.length = maxstr ; /* p.value.address already points to &value */
  CALLYDB( ydb_cip( &ydborder, "^Capital(\"\")", &p_value, &err ));
  if (0 != strlen( err )) fprintf( stdout, "ydborder failure: [%s]\n", err);
  else {
    for (i = 0 ; i < (int) p_value.length ; i++ ) fprintf( stdout, "%c", *(p_value.address+i));
    fprintf( stdout, "\n" );
  };

  /* Ordering through subscripts - next subscript */
  p_value.length = maxstr ; /* p.value.address already points to &value */
  CALLYDB( ydb_cip( &ydborder, "^Capital(\"England\")", &p_value, &err ));
  if (0 != strlen( err )) fprintf( stdout, "ydborder failure: [%s]\n", err);
  else {
    for (i = 0 ; i < (int) p_value.length ; i++ ) fprintf( stdout, "%c", *(p_value.address+i));
    fprintf( stdout, "\n" );
  };

  /* Ordering through nodes - next node */
  p_value.length = maxstr ; /* p.value.address already points to &value */
  CALLYDB( ydb_cip( &ydbquery, "^Capital(\"England\")", &p_value, &err ));
  if (0 != strlen( err )) fprintf( stdout, "ydbquery failure: [%s]\n", err);
  else {
    for (i = 0 ; i < (int) p_value.length ; i++ ) fprintf( stdout, "%c", *(p_value.address+i));
    fprintf( stdout, "\n" );
  };

  /* Kill a (sub)tree */
  CALLYDB( ydb_cip( &ydbkill, "^Capital", &err ));
  if (0 != strlen( err )) fprintf( stdout, "ydbkill failure: [%s]\n", err);

  /* Get an M lock */
  CALLYDB( ydb_cip( &ydblock, "+^CIDemo($Job)", &err ));
  if (0 != strlen( err )) fprintf( stdout, "ydblock failure: [%s]\n", err);

  /* Xecute a string */
  CALLYDB( ydb_cip( &ydbxecute, "write $zyrelease,! zshow \"l\"", &err ));
  if (0 != strlen( err )) fprintf( stdout, "ydbxecute failure: [%s]\n", err);

  return 0;

}
