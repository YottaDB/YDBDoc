#################################################################
#								#
# Copyright (c) 2025 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

CC = cc
CFLAGS = -I${ydb_dist} -Wall -Wextra -g
LDFLAGS = -L${ydb_dist}
LIBYDB = -lyottadb
LIBCURL = -lcurl

users: users.c
	$(CC) $(CFLAGS) users.c -o users $(LDFLAGS) $(LIBYDB) $(LIBCURL)

simple: simple.c
	$(CC) $(CFLAGS) simple.c -o simple $(LDFLAGS) $(LIBYDB)

encode: encode.c
	$(CC) $(CFLAGS) encode.c -o encode $(LDFLAGS) $(LIBYDB)

invalid: invalid.c
	$(CC) $(CFLAGS) invalid.c -o invalid $(LDFLAGS) $(LIBYDB)

longkey: longkey.c
	$(CC) $(CFLAGS) longkey.c -o longkey $(LDFLAGS) $(LIBYDB)

longvalue: longvalue.c
	$(CC) $(CFLAGS) longvalue.c -o longvalue $(LDFLAGS) $(LIBYDB)

clean:
	rm -f users simple invalid longkey longvalue
