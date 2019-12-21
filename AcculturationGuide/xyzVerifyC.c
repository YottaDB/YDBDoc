/* Verify ACID properties of a backed up database */
/* This program is part of the exercises in the YottaDB Acculturation Workshop at
   https://docs.yottadb.com/AcculturationGuide/acculturation.html and its use is discussed there. */
/* For the sake of simplicity, this program does no error handling. */
#include <stdio.h>
#include <string.h>
#include "libyottadb.h"

#define YDB_MAXINT_DIGITS 18 /* maximum size of an integer in YottaDB */
#define YDB_MAXINT_BUFSIZ YDB_MAXINT_DIGITS+2 /* buffer for maximum integer, including sign & null byte */

int rCrab, rDelta, rHorse; /* return values of Simple API calls to access nodes of ^Crab(), ^Delta(), and ^Horse() */
unsigned long long sDeltai=0, tHorsei;
ydb_buffer_t Crab, Delta, Horse,
	tCrab[1], tDelta[1], tHorse[1], /* Arrays of buffers used to pass subscripts */
	vCrab, vDelta, vHorse; /* Buffers used to pass values */

int main()
{
	/* Initialization */
	YDB_LITERAL_TO_BUFFER("^Crab", &Crab);
	YDB_LITERAL_TO_BUFFER("^Delta", &Delta);
	YDB_LITERAL_TO_BUFFER("^Horse", &Horse);
	tCrab[0].buf_addr = malloc(YDB_MAXINT_BUFSIZ);
	*tCrab[0].buf_addr = '0';
	tCrab[0].len_alloc = YDB_MAXINT_BUFSIZ;
	tCrab[0].len_used = 1;
	tDelta[0].buf_addr = malloc(YDB_MAXINT_BUFSIZ);
	*tDelta[0].buf_addr = '0';
	tDelta[0].len_alloc = YDB_MAXINT_BUFSIZ;
	tDelta[0].len_used = 1;
	tHorse[0].buf_addr = malloc(YDB_MAXINT_BUFSIZ);
	*tHorse[0].buf_addr = '0';
	tHorse[0].len_alloc = YDB_MAXINT_BUFSIZ;
	tHorse[0].len_used = 1;
	vCrab.buf_addr = malloc(YDB_MAXINT_BUFSIZ);
	vCrab.len_alloc = YDB_MAXINT_BUFSIZ;
	vDelta.buf_addr = malloc(YDB_MAXINT_BUFSIZ);
	vDelta.len_alloc = YDB_MAXINT_BUFSIZ;
	vHorse.buf_addr = malloc(YDB_MAXINT_BUFSIZ);
	vHorse.len_alloc = YDB_MAXINT_BUFSIZ;
	/* Iterate over nodes in database and verify ACID properties */
	do {
		/* find next nodes; if last node of all three global variables reached without any errors, quit signaling success */
		rCrab = ydb_subscript_next_s(&Crab, 1, &tCrab[0], &tCrab[0]);
		rDelta = ydb_subscript_next_s(&Delta, 1, &tDelta[0], &tDelta[0]);
		rHorse = ydb_subscript_next_s(&Horse, 1, &tHorse[0], &tHorse[0]);
		if (YDB_ERR_NODEEND == rDelta && YDB_ERR_NODEEND == rCrab && YDB_ERR_NODEEND == rHorse) { break; }
		/* if time stamps of next node of each global variable don't match, quit signaling failure */
		*(tCrab[0].buf_addr + tCrab[0].len_used) = '\000';
		*(tDelta[0].buf_addr + tDelta[0].len_used) = '\000';
		*(tHorse[0].buf_addr + tHorse[0].len_used) = '\000';
		if (tDelta[0].len_used != tCrab[0].len_used || tCrab[0].len_used != tHorse[0].len_used ||
				0 != memcmp(tDelta[0].buf_addr, tCrab[0].buf_addr, tDelta[0].len_used) ||
				0 != memcmp(tCrab[0].buf_addr, tHorse[0].buf_addr, tCrab[0].len_used)) {
			printf("ACID fail: tDelta=%s; tCrab=%s; tHorse=%s\n", tDelta[0].buf_addr, tCrab[0].buf_addr, tHorse[0].buf_addr);
			exit(1);
			}
		/* if values at next nodes of ^Crab() and ^Horse() don't match, quit signaling failure */
		vCrab.len_used = 0;
		vHorse.len_used = 0;
		ydb_get_s(&Crab, 1, &tCrab[0], &vCrab);
		*(vCrab.buf_addr + vCrab.len_used) = '\000';
		ydb_get_s(&Horse, 1, &tHorse[0], &vHorse);
		*(vHorse.buf_addr + vHorse.len_used) = '\000';
		tHorsei = strtoll(vHorse.buf_addr, NULL, 10);
		if (0 != (strtoll(vCrab.buf_addr, NULL, 10) + tHorsei)) {
			printf("ACID fail: ^Crab(%s)=%s; ^Horse(%s)=%s\n", tCrab[0].buf_addr, vCrab.buf_addr, tHorse[0].buf_addr, vHorse.buf_addr);
			exit(2);
		}
		/* if the value at next node of ^Horse() is not the sum of this and all preceding nodes of ^Delta(), quit signaling failure */
		vDelta.len_used = 0;
		ydb_get_s(&Delta, 1, &tDelta[0], &vDelta);
		*(vDelta.buf_addr + vDelta.len_used) = '\000';
		sDeltai += strtoll(vDelta.buf_addr, NULL, 10);
		if (sDeltai != tHorsei) {
			printf("ACID fail: Sum ^Delta(0:%s)=%llu; ^Horse(%s)=%s\n", tDelta[0].buf_addr, sDeltai, tHorse[0].buf_addr, vHorse.buf_addr);
			exit(3);
		}
	} while (1);
	printf("ACID test pass\n");
}
