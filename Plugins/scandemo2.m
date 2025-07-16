; No claim of copyright is made with respect to this program
scandemo2	; Scanning the second key column of a table without and with XREFDATA()
	new beg,end,i,j,k,l,m,n,o,p,ref,result1,result2,result3,result4,result5,result6,result7,result8,sub1,sub2,sub3,x
	do UNXREFSUB^%YDBAIM
	; Create the simulated global with nnodes nodes (i.e., table with nnodes rows)
	kill ^X
	set nnodes=+$zcmdline
	set:'nnodes nnodes=1E5
	set i=0
	for  do  quit:i=nnodes
	. set sub1=$random(1E6),sub2=$random(1E6),sub3=$random(1E6)
	. if '$data(^X(sub1,sub2,sub3)),$increment(i) set ^X(sub1,sub2,sub3)=i
	set x=$$XREFSUB^%YDBAIM("^X",3,"1:2",,,,,)
	if "^%ydbAIMSMHzp07CDUHefDG6Nya4054"'=x write "Variable name is ",x," not ^%ydbAIMSMHzp07CDUHefDG6Nya4054",! quit
	set (i,j,k,l,m,n)=0
	set beg=$zut
	set sub1=""
	for  set sub1=$order(^X(sub1)) quit:""=sub1  do
	. set sub2=""
	. for  set sub2=$order(^X(sub1,sub2)) quit:""=sub2  do:750000<sub2
	. . set sub3=""
	. . for  set sub3=$order(^X(sub1,sub2,sub3)) quit:""=sub3  do
	. . . set result1($increment(i))=sub1_"|"_sub2_"|"_sub3
	set end=$zut
	write "Traversal without XREFSUB() took ",$fnumber(end-beg,",")," microseconds for ",$fnumber(i,",")," nodes/rows",!
	set beg=$zut
	set sub2=""
	for  set sub2=$order(^%ydbAIMSMHzp07CDUHefDG6Nya4054(2,sub2)) quit:""=sub2  do:750000<sub2
	. set sub1=""
	. for  set sub1=$order(^%ydbAIMSMHzp07CDUHefDG6Nya4054(2,sub2,sub1)) quit:""=sub1  do
	. . set sub3=""
	. . for  set sub3=$order(^%ydbAIMSMHzp07CDUHefDG6Nya4054(2,sub2,sub1,sub3)) quit:""=sub3  do
	. . . set result2($increment(j))=sub1_"|"_sub2_"|"_sub3
	set end=$zut
	if i'=j write "i=",i," not equal to j=",j,! break
	write "Traversal with XREFSUB() took ",$fnumber(end-beg,",")," microseconds for ",$fnumber(j,",")," nodes/rows",!
	do UNXREFSUB^%YDBAIM(x)
	; Demonstrate string collation.
	set x=$$XREFSUB^%YDBAIM("^X",3,"1:2",,,,,1)
	if "^%ydbAIMSsG49DBaDKylf2LtoHDfwDK"'=x write "Variable name is ",x," not %ydbAIMSsG49DBaDKylf2LtoHDfwDK",! quit
	set beg=$zut
	set sub1=""
	for  set sub1=$order(^X(sub1)) quit:""=sub1  do
	. set sub2=""
	. for  set sub2=$order(^X(sub1,sub2)) quit:""=sub2  do:"#750000"']("#"_sub2)
	. . set sub3=""
	. . for  set sub3=$order(^X(sub1,sub2,sub3)) quit:""=sub3  do
	. . . set result3($increment(k))=sub1_"|"_sub2_"|"_sub3
	set end=$zut
	write "Traversal without XREFSUB() using string collation took ",$fnumber(end-beg,",")," microseconds for ",$fnumber(k,",")," nodes/rows",!
	set beg=$zut
	set ref=""
	for  set ref=$order(^%ydbAIMSsG49DBaDKylf2LtoHDfwDK(2,ref)) quit:""=ref  do:"#750000"']ref
	. set sub1=""
	. for  set sub1=$order(^%ydbAIMSsG49DBaDKylf2LtoHDfwDK(2,ref,sub1)) quit:""=sub1  do
	. . set sub2=$zextract(ref,2,$zlength(ref)),sub3=""
	. . for  set sub3=$order(^%ydbAIMSsG49DBaDKylf2LtoHDfwDK(2,ref,sub1,sub2,sub3)) quit:""=sub3  do
	. . . set result4($increment(l))=sub1_"|"_sub2_"|"_sub3
	set end=$zut
	write "Traversal with XREFSUB() using string collation took ",$fnumber(end-beg,",")," microseconds for ",$fnumber(l,",")," nodes/rows",!
	do UNXREFSUB^%YDBAIM(x)
	; Demonstrate 1:1 transformation function
	set x=$$XREFSUB^%YDBAIM("^X",3,"1:2",,,,2,"$$FUNC^%SQROOT()")
	if "^%ydbAIMSZPyzbXe7LW65H45Uyo3ACL"'=x write "Variable name is ",x," not ^%ydbAIMSZPyzbXe7LW65H45Uyo3ACL",! quit
	set beg=$zut
	set sub1=""
	for  set sub1=$order(^X(sub1)) quit:""=sub1  do
	. set sub2=""
	. for  set sub2=$order(^X(sub1,sub2)) quit:""=sub2  do:750<$$FUNC^%SQROOT(sub2)
	. . set sub3=""
	. . for  set sub3=$order(^X(sub1,sub2,sub3)) quit:""=sub3  do
	. . . set result5($increment(m))=sub1_"|"_sub2_"|"_sub3
	set end=$zut
	write "Traversal without XREFSUB() using a 1:1 transformation function took ",$fnumber(end-beg,",")," microseconds for ",$fnumber(m,",")," nodes/rows",!
	set beg=$zut
	set ref=""
	for  set ref=$order(^%ydbAIMSZPyzbXe7LW65H45Uyo3ACL(2,ref)) quit:""=ref  do:750<ref
	. set sub1=""
	. for  set sub1=$order(^%ydbAIMSZPyzbXe7LW65H45Uyo3ACL(2,ref,sub1)) quit:""=sub1  do
	. . set sub2=$order(^%ydbAIMSZPyzbXe7LW65H45Uyo3ACL(2,ref,sub1,"")),sub3=""
	. . for  set sub3=$order(^%ydbAIMSZPyzbXe7LW65H45Uyo3ACL(2,ref,sub1,sub2,sub3)) quit:""=sub3  do
	. . . set result6($increment(n))=sub1_"|"_sub2_"|"_sub3
	set end=$zut
	write "Traversal with XREFSUB() using a 1:1 transformation function took ",$fnumber(end-beg,",")," microseconds for ",$fnumber(n,",")," nodes/rows",!
	do UNXREFSUB^%YDBAIM(x)
	; Demonstrate many:1 transformation function
	set x=$$XREFSUB^%YDBAIM("^X",3,"1:2",,,,2,"$$sumdigit^scandemo2()")
	if "^%ydbAIMSYNCaawifF3DL8d7wjKLPA3"'=x write "Variable name is ",x," not ^%ydbAIMSYNCaawifF3DL8d7wjKLPA3",! quit
	set beg=$zut
	set sub1=""
	for  set sub1=$order(^X(sub1)) quit:""=sub1  do
	. set sub2=""
	. for  set sub2=$order(^X(sub1,sub2)) quit:""=sub2  do:30<$$sumdigit(sub2)
	. . set sub3=""
	. . for  set sub3=$order(^X(sub1,sub2,sub3)) quit:""=sub3  do
	. . .  set result7($increment(o))=sub1_"|"_sub2_"|"_sub3
	set end=$zut
	write "Traversal without XREFSUB() using a many:1 transformation function took ",$fnumber(end-beg,",")," microseconds for ",$fnumber(o,",")," nodes/rows",!
	set beg=$zut
	set ref=""
	for  set ref=$order(^%ydbAIMSYNCaawifF3DL8d7wjKLPA3(2,ref)) quit:""=ref  do:30<ref
	. set sub1=""
	. for  set sub1=$order(^%ydbAIMSYNCaawifF3DL8d7wjKLPA3(2,ref,sub1)) quit:""=sub1  do
	. . set sub2=""
	. . for  set sub2=$order(^%ydbAIMSYNCaawifF3DL8d7wjKLPA3(2,ref,sub1,sub2)) quit:""=sub2  do
	. . . set sub3=""
	. . . for  set sub3=$order(^%ydbAIMSYNCaawifF3DL8d7wjKLPA3(2,ref,sub1,sub2,sub3)) quit:""=sub3  do
	. . . . set result8($increment(p))=sub1_"|"_sub2_"|"_sub3
	set end=$zut
	write "Traversal with XREFSUB() using a many:1 transformation function took ",$fnumber(end-beg,",")," microseconds for ",$fnumber(p,",")," nodes/rows",!
	quit
sumdigit(n)	; Sum digits of a number
	new i,digit,sum
	set sum=0
	for i=1:1 set digit=$zextract(n,i) quit:""=digit  if $increment(sum,digit)
	quit sum
