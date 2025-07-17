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
#include <string.h>	/* for "strtok" */
#include <ctype.h>	/* for "toupper" */
#include <curl/curl.h>	/* for retrieving JSON content from URL with curl */


#define USERS_INIT_BUF_SIZE (1)
#define USERS_MAX_BUF_SIZE (4 * 1024 * 1024)

static size_t write_callback(char *ptr, size_t size, size_t nmemb, void *userdata) {
	ydb_buffer_t *response = userdata;
	char *buf;
	size_t data_size, buf_size;

	data_size = size * nmemb;
	// Dynamically resize response buffer to accomodate incoming data
	if (data_size > (response->len_alloc - response->len_used)) {
		buf_size = response->len_alloc * 2;
		while (data_size > (buf_size - response->len_used)) {
			buf_size *= 2;
			if (buf_size > USERS_MAX_BUF_SIZE) {
				return 0;
			}
		}
		buf = malloc((buf_size + 1) * sizeof(char)); // allocate null terminator
		memcpy(buf, response->buf_addr, response->len_used);
		free(response->buf_addr);
		response->buf_addr = buf;
		response->len_alloc = buf_size;
	}
	// Append incoming data to response buffer
	memcpy(response->buf_addr + response->len_used, ptr, data_size);
	response->len_used += data_size;

	return data_size;
}

static CURLcode get_url(const char *url, ydb_buffer_t *response, char *curl_error) {
    CURLcode ret;
    CURL *curl;

    curl= curl_easy_init();

    curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, curl_error);
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, response);

    ret = curl_easy_perform(curl);

    curl_easy_cleanup(curl);

    return ret;
}

int main() {
	int		status, done;
	ydb_buffer_t	response, variable, data;
	CURLcode	result;
	char		curl_error[CURL_ERROR_SIZE], ydb_error[YDB_MAX_ERRORMSG];
	const char	*format = "JSON";
	const char	*vn = "^usersJSON";
	const char	*url = "https://randomuser.me/api/?results=5";

	// Get some simple JSON from the Internet
	YDB_MALLOC_BUFFER(&response, USERS_INIT_BUF_SIZE + 1)
	response.len_alloc -= 1; // reserve null terminator
	result = get_url(url, &response, curl_error);
	if (result != 0) {
		puts(curl_error);
		YDB_FREE_BUFFER(&response);
		return 1;
	}
	response.buf_addr[response.len_used] = '\0';
	printf("# JSON response from %s:\n", url);
	fwrite(response.buf_addr, 1, response.len_used, stdout);
	printf("\n\n");

	status = ydb_init();
	if (YDB_OK != status) {
		ydb_zstatus(ydb_error, YDB_MAX_ERRORMSG);
		printf("--> Error: ydb_init() [%s:%d] : %s\n", __FILE__, __LINE__, ydb_error);
		YDB_FREE_BUFFER(&response);
		return 1;
	}

	// Initialize YDB buffers for storing JSON in YottaDB
	YDB_MALLOC_BUFFER(&variable, YDB_MAX_IDENT);
	printf("# Decode JSON response into YottaDB  variable (%s)...\n", vn);
	YDB_COPY_STRING_TO_BUFFER(vn, &variable, done);
	YDB_ASSERT(done);
	status = ydb_decode_s(&variable, 0, NULL, format, response.buf_addr);
	YDB_FREE_BUFFER(&response);
	if (YDB_OK != status) {
		ydb_zstatus(ydb_error, YDB_MAX_ERRORMSG);
		printf("--> Error: ydb_decode_s() [%s:%d] : %s\n", __FILE__, __LINE__, ydb_error);
		YDB_FREE_BUFFER(&variable);
		return 1;
	}

	printf("# Encode data from YottaDB variable (%s) into JSON...\n", vn);
	YDB_MALLOC_BUFFER(&data, YDB_MAX_STR + 1);
	data.len_alloc -= 1; // reserve null terminator
	status = ydb_encode_s(&variable, 0, NULL, format, &data);
	YDB_FREE_BUFFER(&variable);
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
