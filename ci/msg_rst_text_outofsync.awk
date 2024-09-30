#################################################################
#								#
# Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

# This is a helper awk program used by "msg_rst_text_outofsync.csh".
# See "Stage 2" comment in that file for more details on what this program does.

BEGIN	{
	# The below .msg patterns will be checked for a matching xxxx etc. syntax in the .rst file
	# And treated as equivalent.
	msgpattern["!AD"] = "";
	msgpattern["!UL"] = "";
	msgpattern["!2UL"] = "";
	msgpattern["!XL"] = "";
	msgpattern["!SL"] = "";
	msgpattern["!UJ"] = "";
	msgpattern["!AZ"] = "";
	msgpattern["0x!XJ"] = "";
	msgpattern["0x!XL"] = "";
	msgpattern["0x!XW"] = "";
	msgpattern["!@UQ"] = "";
	msgpattern["!@ZQ"] = "";
	msgpattern["!16@XQ"] = "";
	# In some cases we need to allow for "xxxx" to be treated as equivalent to "[!AD]" or "0x!16@XQ" etc.
	# so we allow for "[]" or "0x" addition to all the above .msg patterns.
	for (pattern in msgpattern)
	{
		msgtwin["[" pattern "]"] = ""
		msgtwin["0x" pattern ] = ""
	}
	for (pattern in msgtwin)
		msgpattern[pattern] = ""
	# The below .msg patterns have no equivalent in the .rst syntax so skip them
	msgskipArray["!_"] = ""
	msgskipArray["!/"] = ""
	msgskipArray["\t"] = ""
	newRstTextSubs = 0;
	ARGC = 1;
}

(NR % 3 == 1)	{
	error = $0;
}

(NR % 3 == 2)	{
	rstline = $0;
	rstindex = 1;
	rstmax = length(rstline);
}

(NR % 3 == 0)	{
	msgline = $0;
	msgmax = length(msgline);
	newRstText = "";
	for (j = 1; j <= msgmax; j++)
	{
		msgmatch = 0;
		msgskip = 0;
		for (pattern in msgpattern)
		{
			if (substr(msgline, j, length(pattern)) == pattern)
			{
				msgmatch = 1;
				break;
			}
		}
		if (0 == msgmatch)
		{
			for (pattern in msgskipArray)
			{
				if (substr(msgline, j, length(pattern)) == pattern)
				{
					msgskip = 1;
					break;
				}
			}
		}
		if (msgmatch)
		{
			# Find matching rst pattern (length=5 first, length=4 next, length=3 last)
			# This allows for "xxxxx", "xxxx" and "xxx" usages to be treated as equivalent to "!AD" etc.
			# But not "xxxxxx", "xxxxxxx" etc. or "xx".
			rstmatch = 0;
			for (r = rstindex; (r <= rstmax) && !rstmatch; r++)
			{
				for (rstlen = 5; (rstlen >= 3) && !rstmatch; rstlen--)
				{
					char = substr(rstline, r, 1)
					for (k = 2; k <= rstlen; k++)
					{
						if (substr(rstline, r + k - 1, 1) != char)
							break;
					}
					if (k <= rstlen)
						continue;
					replacement = substr(rstline, r, rstlen);
					rstindex = r + rstlen;
					rstmatch = 1;
					j = j + length(pattern) - 1;
				}
			}
			if (!rstmatch)
			{
				replacement = substr(msgline, j, 1)
				printf "ERROR-E-MISSINGRSTPARAMS : Error [%s] pattern [%s] in .msg has no matching xxxx parameter in .rst\n", error, pattern;
				msgerror = 1;
			}
		} else if (msgskip)
		{
			replacement = "";
			j = j + length(pattern) - 1;
		} else {
			replacement = substr(msgline, j, 1);
		}
		newRstText = newRstText replacement;
	}
	if ((rstline != newRstText) && !msgerror) {
		newRstTextArray[error] = newRstText;
		newRstTextSubs++;
	}
}

END	{
	if (msgerror) {
		exit 1;
	}
	if (0 == newRstTextSubs) {
		exit 0;
	}
	inputfile = ARGV[1];
	outputfile = inputfile ".new"
	while (0 < (getline oldrstline < inputfile)) {
		commaindex = index(oldrstline, ", ");
		if (0 != commaindex) {
			errname = substr(oldrstline, 1, commaindex - 1);
			if (errname in newRstTextArray) {
				printf "%s, %s\n", errname, newRstTextArray[errname] >> outputfile;
			} else {
				commaindex = 0;
			}
		}
		if (0 == commaindex) {
			print oldrstline >> outputfile;
		}
	}
	close(inputfile);
}
