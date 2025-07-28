/****************************************************************
 *								*
 * Copyright (c) 2025 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include "libyottadb.h"	/* for ydb_* macros/prototypes/typedefs */

#include <sys/types.h>	/* needed for "getpid" */
#include <unistd.h>	/* needed for "getpid" */
#include <stdio.h>	/* for "printf" */

#define NUM_SUBS 2

int main() {
	int		i, status, done;
	ydb_buffer_t	variable, subscripts[NUM_SUBS], data;
	char		ydb_error[YDB_MAX_ERRORMSG];
	char		*sub_names[] = {"sub1", "sub2"};
	const char	*format = "JSON";
	const char	*vn = "^encodeJSON";

	status = ydb_init();
	if (YDB_OK != status) {
		ydb_zstatus(ydb_error, YDB_MAX_ERRORMSG);
		printf("--> Error: ydb_init() [%s:%d] : %s\n", __FILE__, __LINE__, ydb_error);
		return 1;
	}

	// Initialize YDB buffers for retrieving JSON from YottaDB
	YDB_MALLOC_BUFFER(&variable, YDB_MAX_IDENT);
	YDB_COPY_STRING_TO_BUFFER(vn, &variable, done);
	YDB_ASSERT(done);
	for (i = 0; i < NUM_SUBS; i++) {
		YDB_MALLOC_BUFFER(&subscripts[i], YDB_MAX_STR);
		YDB_COPY_STRING_TO_BUFFER(sub_names[i], &subscripts[i], done);
		YDB_ASSERT(done);
	}
	YDB_MALLOC_BUFFER(&data, YDB_MAX_STR + 1);
	data.len_alloc -= 1; // reserve null terminator

	printf("# Encode data from YottaDB variable (%s) into JSON...\n", vn);
	status = ydb_encode_s(&variable, NUM_SUBS, subscripts, format, &data);
	YDB_FREE_BUFFER(&variable);
	for (i = 0; i < NUM_SUBS; i++) {
		YDB_FREE_BUFFER(&subscripts[i]);
	}
	if (YDB_OK != status) {
		ydb_zstatus(ydb_error, YDB_MAX_ERRORMSG);
		printf("--> Error: ydb_encode_s() [%s:%d] : %s\n", __FILE__, __LINE__, ydb_error);
		YDB_FREE_BUFFER(&data);
		return 1;
	}
	printf("# Print the encoded JSON:\n");
	fwrite(data.buf_addr, 1, data.len_used, stdout);
	printf("\n\n");
	YDB_FREE_BUFFER(&data);


	return YDB_OK;
}


