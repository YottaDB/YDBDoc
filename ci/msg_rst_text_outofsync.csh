#!/usr/bin/env tcsh
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

# This script checks the following (from bullet 5 of YDBDoc#409 issue description)
#
# Check that the 1-line message text in `YDBDoc/MessageRecovery/errors.rst` for each message is in sync
# with the message text in `YDB/sr_port/*.msg`.
#
set inputfile = "MessageRecovery/errors.rst"
set errlist = `grep -A 1 '^---' $inputfile | grep '^[A-Z]' | grep -vEw "DBRNDWNBYPASS|SIGGORTNTIMEOUT|SIGACKTIMEOUT|ILLEGALUSE|INVALIDGBL"`
set nonomatch

# There are a few error messages that are currently not checked. This is because if they are included, one
# sees an error show up in the below script. The 2 lists are captured in two files below.
# See https://gitlab.com/YottaDB/DB/YDBDoc/-/merge_requests/768#note_2137807850 for more details on this.
# It is the hope that future commits will get the below lists down to zero.

cat > /tmp/msg_rst_text_exceptions.txt << CAT_EOF
BKUPPROGRESS
DBCMPBAD
DBCMPMX
DBCMPNZRO
DBNAMEMISMATCH
ICUVERLT36
JNLREADEOF
JNLWRERR
NAMGVSUBOFLOW
PERMGENDIAG
SRCBACKLOGSTATUS
STRNOTVALID
SYSCALL
CAT_EOF

cat > /tmp/msg_rst_text_missingrstparams.txt << CAT_EOF
AIOBUFSTUCK
ASSERT
BINHDR
BLKSIZ512
BUFOWNERSTUCK
BUFRDTIMEOUT
COLLDATAEXISTS
COMMITWAITPID
CRYPTINIT2
CRYPTKEYFETCHFAILEDNF
DBADDRANGE
DBADDRANGE8
DBBNPNTR
DBBPLMGT2K
DBBPLMLT512
DBBPLNOT512
DBBSIZZRO
DBDANGER
DBFREEZEOFF
DBFREEZEON
DBMISALIGN
DBNULCOL
DBSHMNAMEDIFF
DLRCILLEGAL
DLRCTOOBIG
ENCRYPTCONFLT2
FATALERROR1
FATALERROR2
FILEIDGBLSEC
FILENOTFND
FOROFLOW
FREEMEMORY
GTMASSERT2
GTMSECSHRSRVF
HTOFLOW
INSUFFSUBS
INVDLRCVAL
INVGVPATQUAL
INVNAMECOUNT
INVVALUE
INVVARNAME
JNLALLOCGROW
JNLBUFINFO
JNLCREATE
JNLDBTNNOMATCH
JNLPOOLRECOVERY
JNLQIOSALVAGE
JNLSTATE
JNLSWITCHSZCHG
JNLTRANSGTR
JNLTRANSLSS
KILLBYSIG
KILLBYSIGUINFO
LIBYOTTAMISMTCH
LOWSPACECRE
MAPBAD
MAPDUP
MEMORY
MIXIMAGE
MUFILRNDWNSUC
MUKILLIP
MUPIPSIG
NETFAIL
NOMORESEMCNT
NULSUBSC
OBJNOTADD
OBJNOTCHG
OBJNOTFND
OPCOMMISSED
PARAMINVALID
PREFIXBAD
PROCTERM
READONLYLKFAIL
REPLSTATE
REPLTRANS2BIG
RESYNCSEQLOW
SDSEEKERR
SIMPLEAPINEST
SRCLIN
SUBSARRAYNULL
SUSPENDING
TIME2LONG
TPNOTACID
TPTOODEEP
TRIGDEFBAD
TRIGDEFNOSYNC
VIEWFN
XTRNRETVAL
ZCCONVERT
ZGBLDIRACC
ZTRIGINVACT
CAT_EOF

#######################
# Stage 1
#######################
# Identify list of messages whose text differ between .rst and .msg files.
# Messages that have no parameters will get filtered out by this simple process.
set outfile = "/tmp/msg_rst_text_outofsync.out"
rm -f $outfile
touch $outfile
foreach error ($errlist)
	set skip = 0
	foreach file (/tmp/msg_rst_text_missingrstparams.txt /tmp/msg_rst_text_exceptions.txt)
		grep -w $error $file >& /dev/null
		if (! $status) then
			set skip = 1
			break
		endif
	end
	if ($skip) then
		continue
	endif
	echo "${error}" >> $outfile
	# Take care NOT to redirect the grep/sed output below to tcsh variables using backquotes.
	# Double spaces will be trimmed down to 1 space by tcsh (a tcsh misfeature). Hence backquotes are not used below.
	grep "^$error,[[:space:]]" $inputfile | sed 's/^[A-Z0-9_]*, //;' >> $outfile
	grep "^${error}[[:space:]]" ../YDB/sr_port/*.msg | awk -F'\t*<|>' '{print $2}' >> $outfile
end
unset nonomatch

#######################
# Stage 2
#######################
# Pass list of messages from Stage 1 to an AWK program that will do a fancy check of whether the two
# messages are really out of sync. For example, the below two versions are clearly different
# but would be identified as equal in Stage 2 because "xxxx" and "!AD" are considered equal etc.
#
# .rst version : ACOMPTBINC, Deviceparameter xxxx is compatible with only yyyy in the command zzzz
# .msg version : ACOMPTBINC, Deviceparameter !AD is compatible with only !AD in the command !AD
#
# This AWK program not only identifies messages that are out of sync inspite of treating "xxxx" and "!AD"
# as equivalent (note that this is just one example of many cases which are treated equivalent by the
# M program) but also produces a ".rst" file that is modified to bring it in sync with the ".msg" files.
# The file "errors.rst" is modified to fix all the outofsync stuff and can later be run through "git commit".
# Nevertheless, the script will exit with a non-zero exit status in this case to ensure the pipeline fails
# forcing the user to fix this failure by getting the .rst back in sync with the .msg.
#
set errfile = "/tmp/msg_rst_text_outofsync.err"
rm -f $inputfile.new
cat $outfile | awk -f ci/msg_rst_text_outofsync.awk $inputfile > $errfile
if (! -z $errfile) then
	cat $errfile
	exit -1
endif

if (-e $inputfile.new) then
	echo "[$inputfile] has been automatically updated due to concurrent changes in YDB/sr_port/*.msg files."
	echo "Below is the [git diff] of the changes."
	echo "Please run [git commit] with this changes and rerun this script."
	echo "Exiting with non-zero status for now."
	mv $inputfile.new $inputfile
	git diff
	exit -1
endif

exit 0

