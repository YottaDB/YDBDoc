/****************************************************************
 *								*
 * Copyright (c) 2025-2026 YottaDB LLC and/or its subsidiaries.	*
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
	ydb_buffer_t	variable, subscripts[NUM_SUBS];
	ydb_string_t	json_output;
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

	printf("# Encode data from YottaDB variable (%s) into JSON...\n", vn);
	status = ydb_encode_s(&variable, NUM_SUBS, subscripts, format, &json_output);
	YDB_FREE_BUFFER(&variable);
	for (i = 0; i < NUM_SUBS; i++) {
		YDB_FREE_BUFFER(&subscripts[i]);
	}
	if (YDB_OK != status) {
		ydb_zstatus(ydb_error, YDB_MAX_ERRORMSG);
		printf("--> Error: ydb_encode_s() [%s:%d] : %s\n", __FILE__, __LINE__, ydb_error);
		return 1;
	}
	printf("# Print the encoded JSON:\n");
	fwrite(json_output.address, 1, json_output.length, stdout);
	printf("\n\n");
	free(json_output.address);

	return YDB_OK;
}


