waitforhrtbt
    ; This routine returns 0 when the acknowledged seqno from the specified instance matches or exceeds the specified seqno
    ; If there is no confirmation (network issues etc) from the Receiver Server for 300 seconds, this routines returns 1.
    ; Usage: $gtm_exe/mumps -r ^waitforhrtbt <instname> <seqnocheckpoint>
    set $etrap="write ""WAITFORHRTBT-E-ERROR, Error occurred while waiting for ackseqno confirmation due to "",$zstatus  halt  "
    new heartbeatseqno,hangduration,instance,checkseqno,i,instname,slot
    ; set $ztimeout to align with the REPLALERT threshold for the test system
    set $ztimeout="900:write 1 halt"
    set hangduration=1,slot=""
    set instance=$piece($zcmdline," ",1)
    set checkseqno=$piece($zcmdline," ",2)
    if '($length(instance)&$length(checkseqno)) write "WAITFORHRTBT-E-ARGS : ",$zcmdline," was specified. This routine requires specifying instance name and seqno.",! halt
    for i=0:1:15 set instname=$$^%PEEKBYNAME("gtmsource_local_struct.secondary_instname",i),instname=$piece(instname,$char(0),1) set:(instname=instance) slot=i
    if '($length(slot)) write "WAITFORHRTBT-E-INSTNOMATCH : No matching instance name for ",instance,! halt
    for  do
    . set heartbeatseqno=$$^%PEEKBYNAME("gtmsource_local_struct.heartbeat_jnl_seqno",slot,"I")-1
    . if (heartbeatseqno>=checkseqno) write 0 halt
    . hang hangduration
    quit

