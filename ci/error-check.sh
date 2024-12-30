#!/bin/sh
 ###############################################################
 #                                                             #
 # Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.     #
 # All rights reserved.                                        #
 #                                                             #
 #     This source code contains the intellectual property     #
 #     of its copyright holder(s), and is made available       #
 #     under a license.  If you do not know the terms of       #
 #     the license, please stop and do not read further.       #
 #                                                             #
 ###############################################################

# This section of script responsible for detecting undocumented or incorrectly documented messages
# More details on https://gitlab.com/YottaDB/DB/YDBDoc/-/issues/409

# Check if every message documented in errors.rst is also documented with one error number
# (or more error numbers in rare cases) in errormsgref.rst
grep -A 1 '^---' MessageRecovery/errors.rst | grep '^[A-Z]' | grep -vwE "ILLEGALUSE|INVALIDGBL" > /tmp/err_errors.out
grep '^| [0-9]' MessageRecovery/errormsgref.rst | sed 's/^| [0-9][0-9]* | //;s/,.*//g;' | sort -u > /tmp/err_errormsgref.out
if ! diff /tmp/err_errors.out /tmp/err_errormsgref.out > err_diff.out; then
	echo "FATAL: some error message documented with error numbers/more error numbers in errormsgref.rst"
	echo "Please check diff output below:"
	cat err_diff.out
	exit 1
else
	rm err_diff.out
fi

# Check if Every message number recorded in errormsgref.rst is accurate
# (i.e. is aligned with the number in the YDB/sr_port/ydb*errors.h files in the YDB project
grep "#define" ../YDB/sr_port/ydb*errors.h | grep -v '#define CMI_REASON_' | grep -vE "CMI_BADPORT|CMI_NOTND|CMI_OVERRUN|CMI_NOSERVENT|CMI_BADIPADDRPORT|ERR_ACK|ERR_ASC2EBCDICCONV|ERR_BADTAG|ERR_DBGLDMISMATCH|ERR_DRVLONGJMP|ERR_ENQ|ERR_FAKENOSPCLEARED|ERR_FREEZEID|ERR_INVDBGLVL|ERR_JNLREQUIRED|ERR_JNLWRTNOWWRTR|ERR_JOBINTRRETHROW|ERR_JOBINTRRQST|ERR_KRNLKILL|ERR_LVMONBADVAL|ERR_MUDESTROYFAIL|ERR_MUDESTROYSUC|ERR_REPEATERROR|ERR_REPLONLNRLBK|ERR_TPRETRY|ERR_WILLEXPIRE|ERR_YDIRTSZ|ERR_ZDEFACTIVE|ERR_ZDEFOFLOW|ERR_ZLINKBYPASS|ERR_UNUSEDMSG" | sed 's/.*#define CMERR_// ; s/.*#define CMI_// ; s/.*#define ERR_// ; s/.*#define GDE_// ; ' | awk '{print $2,$1}' | sort > /tmp/err_ydberror.out
grep '| [0-9]' MessageRecovery/errormsgref.rst | grep -vEw "DBRNDWNBYPASS|SIGGORTNTIMEOUT|SIGACKTIMEOUT" | sed 's/,.*//;' | awk '{print $2,$4}' | sort > /tmp/err_errormsgref.out
if ! diff /tmp/err_ydberror.out /tmp/err_errormsgref.out > err_diff2.out; then
	echo "FATAL: message number recorded in errormsgref.rst is NOT accurate"
	echo "Please check diff output below:"
	cat err_diff2.out
	exit 1
else
	rm err_diff2.out
fi

# Check of 1-line message text in errors.rst for each message is
# in sync with the message text in YDB/sr_port/*.msg in the YDB project,
# and with the message text in errormsgref.rst.
ci/error_sync.py || exit 1
