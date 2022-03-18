/* Update a multi-region database using ACID transactions */
/* This program is one of the exercises in the YottaDB Acculturation Workshop at
   https://docs.yottadb.com/AcculturationGuide/acculturation.html and its use is
   discussed there. */
/* For the sake of simplicity, this program does no error handling. */
#include "libyottadb.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#define YDB_MAXINT_DIGITS 18			/* maximum size of an integer in YottaDB */
#define YDB_MAXINT_BUFSIZ YDB_MAXINT_DIGITS + 2 /* buffer for maximum integer, including sign & null byte */

struct timespec systime;
long long	vali;					/* Integer values passed between application and YottaDB */
unsigned char	currtime[YDB_MAXINT_BUFSIZ],		/* string representation of current time */
    rands[YDB_MAXINT_BUFSIZ],				/* string representation of random number */
    tmp1s[YDB_MAXINT_BUFSIZ], tmp2s[YDB_MAXINT_BUFSIZ]; /* temporary strings representing integers */
unsigned long long randi;
ydb_buffer_t	   Crab, Delta, Horse, /* Buffers for names of global variables used in this application */
    sub1[1],			       /* sub1 & sub2 are used to pass subscripts; just 1 per node in this
					  application */
    sub2[1], valb;		       /* Buffer used to pass values between application and YottaDB */

int do_xyzTrans(void *ignore_me) {
	clock_gettime(CLOCK_REALTIME, &systime);
	snprintf(&currtime[0], YDB_MAXINT_BUFSIZ, "%lu", systime.tv_sec * 1000000 + systime.tv_nsec / 1000);
	sub1[0].len_used = strlen(currtime);
	/* Assigning randi value to ^Delta(currtime) */
	randi = (long long)rand();
	snprintf(&rands[0], YDB_MAXINT_BUFSIZ, "%u", randi);
	valb.buf_addr = &rands[0];
	valb.len_used = strlen(rands);
	ydb_set_s(&Delta, 1, &sub1[0], &valb);
	/* Getting previous subscript of ^Horse */
	sub2[0].len_used = 0;
	valb.buf_addr = &tmp1s[0];
	valb.len_used = 0;
	ydb_subscript_previous_s(&Horse, 1, &sub2[0], &valb); /* after call valb refers to last ^Horse subscript */
	sub2[0].buf_addr = valb.buf_addr;
	sub2[0].len_used = valb.len_used;
	valb.buf_addr = &tmp2s[0];
	valb.len_used = 0;
	ydb_get_s(&Horse, 1, &sub2[0], &valb);	   /* after call valb refers to value at last ^Horse node */
	*(valb.buf_addr + valb.len_used) = '\000'; /* insert null byte for string to integer conversion */
	vali = strtoll(valb.buf_addr, NULL, 10);   /* vali contains integer value of last ^Horse node */
	vali += randi;
	/* Assigning updated vali value to ^Horse(currtime) */
	snprintf(valb.buf_addr, YDB_MAXINT_BUFSIZ, "%lli", vali);
	valb.len_used = strlen(valb.buf_addr);
	ydb_set_s(&Horse, 1, &sub1[0], &valb);
	/* Getting previous subscript of ^Crab */
	sub2[0].len_used = 0;
	valb.buf_addr = &tmp1s[0];
	valb.len_used = 0;
	ydb_subscript_previous_s(&Crab, 1, &sub2[0], &valb); /* after call valb refers to last ^Crab subscript */
	sub2[0].buf_addr = valb.buf_addr;
	sub2[0].len_used = valb.len_used;
	valb.buf_addr = &tmp2s[0];
	valb.len_used = 0;
	ydb_get_s(&Crab, 1, &sub2[0], &valb);	   /* after call valb refers to value at last ^Crab node */
	*(valb.buf_addr + valb.len_used) = '\000'; /* insert null byte for string to integer conversion */
	vali = strtoll(valb.buf_addr, NULL, 10);   /* vali contains integer value of last ^Horse node */
	vali -= randi;
	/* Assigning updated vali value to ^Crab(currtime) */
	snprintf(valb.buf_addr, YDB_MAXINT_BUFSIZ, "%lli", vali);
	valb.len_used = strlen(valb.buf_addr);
	ydb_set_s(&Crab, 1, &sub1[0], &valb);
	return YDB_OK;
}

int main() {
	/* Initialization */
	YDB_LITERAL_TO_BUFFER("^Crab", &Crab);
	YDB_LITERAL_TO_BUFFER("^Delta", &Delta);
	YDB_LITERAL_TO_BUFFER("^Horse", &Horse);
	sub1[0].len_alloc = YDB_MAXINT_BUFSIZ; /*	All parameters & values are integers with
						  pre-allocated buffers */
	sub2[0].len_alloc = YDB_MAXINT_BUFSIZ;
	valb.len_alloc = YDB_MAXINT_BUFSIZ;
	sub1[0].buf_addr = &currtime[0]; /* sub1 is used only for the current time,
					    since it is used repeatedly */
	srand(getpid());		 /* seed random number generator with pid */
	/* Infinite loop updating regions in a transaction */
	do {
		ydb_tp_s(*do_xyzTrans, NULL, "BATCH", 0, NULL);
		usleep(500000);
	} while (1);
}
