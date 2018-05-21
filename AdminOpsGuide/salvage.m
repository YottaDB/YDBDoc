salvage;
 read !,"SET REGION to <DEFAULT>: ",r s:r="" r="DEFAULT"
 write !
 set in="db_integ.log",x="mupip integ -fast -nomap -region"
 set teg="/bin/csh -c """_x_" "_r_" >& "_in_""""
 zsystem teg
 set out="db_drive",skip=$char(32,10,13)
 set prefix="map -bl=",old="",lenold=0,blk=0
 open in:(read:exc="goto done"),out:newv
 se out
 write "dse <<yz",!,"find -region=",r,!,"open -file=","db.zwr",!
 use in
 for read x if x["marked" use out do out use in
 ; CAUTION: in the above line, "marked" MUST be in lower-case
 ;
done
 use out
 write "close",!,"change -fileheader -abandoned_kills=0",!
 write "change -fileheader -kill_in_progress=0",!,"exit",!
 write "yz",!
 ;write "mupip load db.zwr",!
 ; uncomment the line above if you do not wish to examine
 ; db.zwr and to initiate the load separately
 close in,out
 zsystem "/usr/local/bin/tcsh -c ""source db_drive"""
 quit
out
 for j=1:1:$length(x) quit:skip'[$extract(x,j)
 set blk=$piece($piece($extract(x,j,999)," ",1),":",1)
 set state=$select($extract(x,42)="f":" -busy",1:" -free")
 ; CAUTION: in the above line, "f" MUST be in lower-case
 if state=" -free" write "dump -zwr -bl=",blk,!
 ; CAUTION: in the above line " -free" MUST match the
 ; case in the $select above
 ; comment out the above line (starting with "i state")
 ; if you wish to eliminate, rather than save,
 ; the contents of loose busy blocks
 write prefix,blk,state,!
quit
