waitforseqnosync
    ; This routine returns 0 when there the sequence numbers of the Source Server and the acknowledged sequence number from the Receiver Server is the same.
    ; It returns a non-zero value when there is no confirmation of the receipt of the latest seqno from the secondary even when there is no backlog.
    ; If there is no confirmation (network issues etc) from the Receiver Server for 150 seconds, this routines returns 1.
    ; Usage: $gtm_exe/mumps -r ^waitforseqnosync <instname>
    set $etrap="write ""WAITFORSEQNOSYNC-E-SRCBACKLOG : unable to get current Source Server backlog and seqno updates status due to "",$zstatus  halt  "
    new readseqno,heartbeatseqno,instance,i,instname,slot,hrtbtperiod
    set $ztimeout="150:write 1 halt"
    set slot=""
    ; hrtbtperiod: the heartbeat period (the fifth parameter of -CONNECTPARAMS)
    set instance=$zcmdline
    if '$length(instance) write "WAITFORSEQNOSYNC-E-ARGS : This routine requires specifying an instance name.",! halt
    for i=0:1:15 set instname=$$^%PEEKBYNAME("gtmsource_local_struct.secondary_instname",i),instname=$piece(instname,$char(0),1) set:(instname=instance) slot=i
    if '($length(slot)) write "WAITFORSEQNOSYNC-E-INSTNOMATCH : No matching instance name for ",instance halt
    set hrtbtperiod=$piece($$^%PEEKBYNAME("gtmsource_local_struct.connect_parms",slot),",",5)
    for  do
    . set seqno=$$^%PEEKBYNAME("jnlpool_ctl_struct.jnl_seqno","","I")
    . set readseqno=$$^%PEEKBYNAME("gtmsource_local_struct.read_jnl_seqno",i,"I")
    . set heartbeatseqno=$$^%PEEKBYNAME("gtmsource_local_struct.heartbeat_jnl_seqno",i,"I")
    . if (seqno=readseqno=heartbeatseqno) write 0 halt
    . hang hrtbtperiod
    quit

