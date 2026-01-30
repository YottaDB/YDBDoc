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

int main() {
	int		status, done;
	ydb_buffer_t	variable;
	ydb_string_t	json_input;
	char		ydb_error[YDB_MAX_ERRORMSG];
	const char	*format = "JSON";
	const char	*vn = "^longkeyJSON";
	char		*JSON = "{\n"
					"\t\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\": \"true\"\n"
				"}";

	status = ydb_init();
	if (YDB_OK != status) {
		ydb_zstatus(ydb_error, YDB_MAX_ERRORMSG);
		printf("--> Error: ydb_init() [%s:%d] : %s\n", __FILE__, __LINE__, ydb_error);
		return 1;
	}

	// Initialize YDB buffers for storing JSON in YottaDB
	YDB_MALLOC_BUFFER(&variable, YDB_MAX_IDENT);
	printf("# Decode JSON into YottaDB variable (%s)...\n", vn);
	YDB_COPY_STRING_TO_BUFFER(vn, &variable, done);
	YDB_ASSERT(done);
	json_input.address = JSON;
	json_input.length = strlen(json_input.address);
	status = ydb_decode_s(&variable, 0, NULL, format, &json_input);
	if (YDB_OK != status) {
		ydb_zstatus(ydb_error, YDB_MAX_ERRORMSG);
		printf("--> Error: ydb_decode_s() [%s:%d] : %s\n", __FILE__, __LINE__, ydb_error);
		YDB_FREE_BUFFER(&variable);
		return 1;
	}

	return YDB_OK;
}

