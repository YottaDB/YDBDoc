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
	const char	*vn = "^simpleJSON";
	const char	*JSON = "{\n"
					"\t\"1\": \"true\",\n"
					"\t\"2\": \"false\",\n"
					"\t\"\": 9,\n"
					"\t\"food\": {\n"
						"\t\t\"kind\": [\n"
							"\t\t\t3,\n"
							"\t\t\t\"oranges\",\n"
							"\t\t\t\"bananas\",\n"
							"\t\t\t\"apples\",\n"
							"\t\t\tnull\n"
						"\t\t],\n"
						"\t\t\"water\": true,\n"
						"\t\t\"type\": {\n"
							"\t\t\t\"color\": \"red\",\n"
							"\t\t\t\"\": 2,\n"
							"\t\t\t\"size\": \"large\"\n"
						"\t\t},\n"
						"\t\t\"plate\": false\n"
					"\t},\n"
					"\t\"three\": \"null\"\n"
				"}";

	status = ydb_init();
	if (YDB_OK != status) {
		ydb_zstatus(ydb_error, YDB_MAX_ERRORMSG);
		printf("--> Error: ydb_init() [%s:%d] : %s\n", __FILE__, __LINE__, ydb_error);
		return 1;
	}

	// Initialize YDB buffers for storing JSON in YottaDB
	YDB_MALLOC_BUFFER(&variable, YDB_MAX_IDENT);
	for (i = 0; i < NUM_SUBS; i++) {
		YDB_MALLOC_BUFFER(&subscripts[i], YDB_MAX_STR);
		YDB_COPY_STRING_TO_BUFFER(sub_names[i], &subscripts[i], done);
		YDB_ASSERT(done);
	}
	printf("# Decode JSON into YottaDB variable (%s)...\n", vn);
	YDB_COPY_STRING_TO_BUFFER(vn, &variable, done);
	YDB_ASSERT(done);
	status = ydb_decode_s(&variable, NUM_SUBS, subscripts, format, JSON);
	if (YDB_OK != status) {
		ydb_zstatus(ydb_error, YDB_MAX_ERRORMSG);
		printf("--> Error: ydb_decode_s() [%s:%d] : %s\n", __FILE__, __LINE__, ydb_error);
		YDB_FREE_BUFFER(&variable);
		for (i = 0; i < NUM_SUBS; i++) {
			YDB_FREE_BUFFER(&subscripts[i]);
		}
		return 1;
	}
	// The database should now contain the following nodes:
	// ^simpleJSON("sub1","sub2")=9
	// ^simpleJSON("sub1","sub2",1)="true"
	// ^simpleJSON("sub1","sub2",2)="false"
	// ^simpleJSON("sub1","sub2","food","kind",0)=3
	// ^simpleJSON("sub1","sub2","food","kind",1)="oranges"
	// ^simpleJSON("sub1","sub2","food","kind",2)="bananas"
	// ^simpleJSON("sub1","sub2","food","kind",3)="apples"
	// ^simpleJSON("sub1","sub2","food","kind",4)=$C(0)_"null"
	// ^simpleJSON("sub1","sub2","food","plate")=$C(0)_"false"
	// ^simpleJSON("sub1","sub2","food","type")=2
	// ^simpleJSON("sub1","sub2","food","type","color")="red"
	// ^simpleJSON("sub1","sub2","food","type","size")="large"
	// ^simpleJSON("sub1","sub2","food","water")=$C(0)_"true"
	// ^simpleJSON("sub1","sub2","three")="null"

	printf("# Encode data from YottaDB variable (%s) into JSON...\n", vn);
	YDB_MALLOC_BUFFER(&data, YDB_MAX_STR + 1);
	data.len_alloc -= 1; // reserve null terminator
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
	// Expect the following JSON output:
	// {"": 9, "1": "true", "2": "false", "food": {"kind": {"0": 3, "1": "oranges", "2": "bananas", "3": "apples", "4": null}, "plate": false, "type": {"": 2, "color": "red", "size": "large"}, "water": true}, "three": "null"}
	fwrite(data.buf_addr, 1, data.len_used, stdout);
	printf("\n\n");
	YDB_FREE_BUFFER(&data);


	return YDB_OK;
}

