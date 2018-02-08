CONVDBKEYS
	; No claim of copyright is made with regard to to this code
	;
	; Convert file in gtm_dbkeys format to libconfig format.  Usage
	; $gtm_dist/mumps -run CONVDBKEYS [source] destination
	;  - if not specified, source is environment variable $gtm_dbkeys
	;  - if source is directory, destination must also be a directory
	;  - if destination is directory, output file(s) has(have) the same name(s) as source file(s)
	; Output files are overwritten
	; Anything on the commandline after destination is ignored.
	;
	; Set error handler to print error message and return error code to shell
	set $etrap="goto ERROR^"_$TEXT(+0)
	set:$stack $ecode=",U255,"	; the top level entryref is only supported when called from the shell with mumps -run
	;
	new dst,dfile,nparm,sfile,spat,src
	use $principal:(ctrap=$char(3))
	set nparm=$length($zcmdline," ")
	if 1<nparm set src=$piece($zcmdline," ",1),dst=$piece($zcmdline," ",2)
	else  if 1=nparm set src=$ztrnlnm("gtm_dbkeys"),dst=$piece($zcmdline," ",1)
	if ""=src set $ecode=$select(""=dst:",U250,",1:",U251,")
	else  set:""=dst $ecode=",U249,"
	if $$ISDIR(src) set:'$$ISDIR(dst) $ecode=",U248," do
	. set:"/"'=$extract(dst,$length(dst)) dst=dst_"/"
	. set spat=src_$select("/"=$extract(src,$length(src)):"",1:"/")_"*"
	. for  set sfile=$zsearch(spat,31) quit:""=sfile  set dfile=dst_$piece($zparse(sfile),$zparse(sfile,"DIRECTORY"),2) do FUNC(sfile,dfile)
	else  do FUNC(src,$select($$ISDIR(dst):dst_$select("/"=$extract(dst,$length(dst)):"",1:"/")_$piece($zparse(src),$zparse(src,"DIRECTORY",2),2),1:dst))
	quit

ERROR	; main error handler for user errors
	 set $etrap="goto ERROR1"
	 use $principal
	 set ecode=$ecode,$ecode=""	; no need to new ecode because process is about to exit
	 set tmp1=$piece(ecode,",",2),tmp2=$text(@(tmp1_"^"_$text(+0)))
	 if $length(tmp2) write $text(+0),@$piece(tmp2,";",2),! zhalt +$extract(tmp1,2,$length(tmp1))
ERROR1	 ; error handler for error in main error handler - fall into this for non-user errors
	 set $etrap="halt"   ; set last chance error handler to just exit process
	 use $principal write $zstatus,! zhalt 1

FUNC(src,dst)
	; src is a file in gtm_dbkeys format
	; dst is a file to be written in libconfig format
	; database and key files in src need not actually exist - this function does not need to be run on the system with the files
	new dbfile,flag,io,keys,line,line1,line2,tab
	set io=$io
	set tab="    "
	open src:readonly use src
	for  read:$increment(line) line1 quit:$zeof  do
	. set line1=$$FUNC^%TRIM($$^%MPIECE(line1))
	. set:"dat"'=$zconvert($piece(line1," ",1),"L") $ecode=",U253,"
	. read:$increment(line) line2 set:$zeof $ecode=",U254,"
	. set line2=$$FUNC^%TRIM($$^%MPIECE(line2))
	. set:"key"'=$zconvert($piece(line2," ",1),"L") $ecode=",U252,"
	. set keys($piece(line1," ",2))=$piece(line2," ",2)
	open dst:newversion use dst close src
	write !,"/* Database encryption section - created by ",$TEXT(+0)," ",$zdate($h,"MON DD, YYYY 24:60:SS")," */",!,!
	write "database: {",!
	write tab,"keys: ("
	set dbfile="",flag=0 for  set dbfile=$order(keys(dbfile)) quit:""=dbfile  do
	. if flag write ","
	. else  set flag=1
	. write !,tab,tab,"{",!,tab,tab,tab,"dat: """,dbfile,""";",!
	. write tab,tab,tab,"key: """,keys(dbfile),""";",!,tab,tab,"}"
	write !,tab,");",!,"};",!
	use io close dst
	quit:$quit 1 quit

ISDIR(path)
	; returns 1 if path is a directory; 0 otherwise
	new tmp1,tmp2
	set tmp1=$zparse(path),tmp2=$zparse(path_"/")
	quit $select(""=tmp1:0,""'=tmp2:1,1:0)

;	Error message texts
U248	;"-F-DSTNOTDIR Source """_src_""" is a directory, but destination """_dst_""" is not"
U249	;"-F-NODST No destination provided"
U250	;"-F-NOSRCEORDST No source or destination provided"
U251	;"-F-NOSRC No source provided on either command line or via $gtm_dbkeys"
U252	;"-F-INVALIDKWD file "_src_" line "_line_" saw """_$piece(line2," ",1)_""" but ""key"" expected"
U253	;"-F-INVALIDKWD file "_src_" line "_line_" saw """_$piece(line1," ",1)_""" but ""dat"" expected"
U254	;"-F-PREMATUREEOF in file "_src_" line "_line_" database file """_line1_""" has no following line to specify a key file"
U255	;"-F-BADINVOCATION Must invoke from shell as mumps -run "_$text(+0)
