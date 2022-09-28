replspeed
    ; This routine returns the replication speed, that is the number of seqno updates per second acknowledged by the replicating instance during heartbeat intervals
    ; Usage: yottadb -run ^replspeed INSTANCE3 20
    ; The second parameter is the sampling size
    set $etrap="write ""REPLSPEED-E-ACKSMPL : unable to fetch sampling data due to "",$zstatus  halt  "
    set $ztimeout="300:write ""Timeout occurred out after 5 minutes"",! zwrite  halt"
    new hrtbtperiod,instance,samplingsize,hrtbts,slot,i,hrtbtdiffs,diff,dump
    set instance=$piece($zcmdline," ",1),slot=0
    set samplingsize=$piece($zcmdline," ",2),hrtbtdiffs=0
    set:$length(samplingsize)=0 samplingsize=10
    if '($length(instance)) write "REPLSPEED-E-ARGS : ",$zcmdline," was specified. This routine requires specifying an instance name.",! halt
    for i=0:1:15 set instname=$$^%PEEKBYNAME("gtmsource_local_struct.secondary_instname",i),instname=$piece(instname,$char(0),1) set:(instname=instance) slot=i
    ; capture heartbeat_jnl_seqno samplingsize times. Wait for hrtbtperiod after every capture of heartbeat_jnl_seqno
    set hrtbtperiod=$piece($$^%PEEKBYNAME("gtmsource_local_struct.connect_parms",slot),",",5)
    for j=1:1:samplingsize do
    . set hrtbts(j)=$$^%PEEKBYNAME("gtmsource_local_struct.heartbeat_jnl_seqno",slot,"I")
    . do:(j>1)
    . . set diff=hrtbts(j)-hrtbts(j-1)
    . . do:(diff<0)
    . . . write "REPLSPEED-E-ACKSEQNO : acknowledgement sequence number received is lower than previous acknowledgement seqno."
    . . . set dump="replspeed.dump" open dump use dump zwrite hrtbts  close dump
    . . . halt
    . . set hrtbtdiffs=hrtbtdiffs+diff
    . hang hrtbtperiod
    set hrtbtdiffs=hrtbtdiffs/samplingsize
    write $justify(hrtbtdiffs/hrtbtperiod,0,0)
    quit
