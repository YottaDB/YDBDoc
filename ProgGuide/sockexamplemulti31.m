; No claim of copyright is made with regard to this code.
; This is example code for explanatory purposes, and is not intended
; for production use.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This program demonstrates a use of $KEY and $ZKEY in a basic socket
; I/O setup. It launches two jobs: a server process which opens a
; listening socket and a client process which makes five connections
; to the server.  The server sends a message to each connection
; socket.  Even-numbered client sockets read the message partially but
; do not send a response back to the server.  Odd-numbered client
; sockets receive the full message and respond to the server with the
; message "Ok.".  The server reads two characters (but the client
; sends three) and $ZKEY shows sockets with unread characters.
;
;
;                     +---------+ <-No response +----------+
;      +--------------+ handle1 +---------------+ client1  |
;      |              +---------+   Full msg -> +----------+
;      |
;      +--------------+---------+ <- "Ok."      +----------+
;      |              | handle2 +---------------+ client2  |
; +----+---+          +---------+Partial msg -> +----------+
; |        |
; | Server |          +---------+ <-No response +----------+
; |        +----------+ handle3 +---------------+ client3  |
; |        |          +---------+  Full msg ->  +----------+
; +----+---+
;      |              +---------+ <- "Ok."      +----------+
;      +--------------+ handle4 +---------------+ client4  |
;      |              +---------+ Partial msg-> +----------+
;      |
;      |              +---------+<- No response +----------+
;      +--------------+ handle5 +---------------+ client5  |
;                     +---------+  Full msg ->  +----------+
;
;+------------------------------++-------------------------+
;        server job                    client job
;
;
; The program displays the stderr and stdout of server and client jobs
; and produces a report such as:
;
; $ mumps -r sockexamplemulti3
;At JAN 28, 2015 05:41:22 waiting for server job 24675 & client job 24677
;At JAN 28, 2015 05:41:25 waiting for server job 24675 & client job 24677
;At JAN 28, 2015 05:41:28 waiting for server job 24675 & client job 24677
;At JAN 28, 2015 05:41:31 waiting for server job 24675 & client job 24677
;At JAN 28, 2015 05:41:34 waiting for server job 24675 & client job 24677
;At JAN 28, 2015 05:41:37 waiting for server job 24675 & client job 24677
;At JAN 28, 2015 05:41:40 waiting for server job 24675 & client job 24677
;At JAN 28, 2015 05:41:43 waiting for server job 24675 & client job 24677
;At JAN 28, 2015 05:41:46 waiting for server job 24675 & client job 24677
;At JAN 28, 2015 05:41:49 waiting for server job 24675 & client job 24677
;At JAN 28, 2015 05:41:52 waiting for server job 24675 & client job 24677
;At JAN 28, 2015 05:41:55 waiting for server job 24675 & client job 24677
;
;Server stderr:
;
;Server stdout:
;** tcpserver is getting ready to start.**
;JAN 28, 2015 05:41:22 Open serverport 2141 successful, $key=LISTENING|server|2141
;JAN 28, 2015 05:41:22 Connection established, socket=h1422441682000, ip=::1
;
;server+43^sockexamplemulti3@63580,20482 readhandle="h1422441682000";
;
;readhandle=h1422441682000, zkey=READ|h1422441682000|::1
;connections("h1422441682000")=1
;
;server+43^sockexamplemulti3@63580,20482 readhandle="h1422441682000"
;JAN 28, 2015 05:41:22 Connection established, socket=h1422441682001, ip=::1
;JAN 28, 2015 05:41:22 Connection established, socket=h1422441682002, ip=::1
;
;server+43^sockexamplemulti3@63580,20482 readhandle="h1422441682002"
;
;readhandle=h1422441682002, zkey=READ|h1422441682002|::1
;connections("h1422441682000")=2
;connections("h1422441682001")=0
;connections("h1422441682002")=1
;
;server+43^sockexamplemulti3@63580,20482 readhandle="h1422441682002"
;JAN 28, 2015 05:41:22 Connection established, socket=h1422441682003, ip=::1
;JAN 28, 2015 05:41:22 Connection established, socket=h1422441682004, ip=::1
;
;server+43^sockexamplemulti3@63580,20482 readhandle="h1422441682004"
;
;readhandle=h1422441682004, zkey=READ|h1422441682004|::1
;connections("h1422441682000")=2
;connections("h1422441682001")=0
;connections("h1422441682002")=2
;connections("h1422441682003")=0
;connections("h1422441682004")=1
;
;server+43^sockexamplemulti3@63580,20482 readhandle="h1422441682004"
;
;server+43^sockexamplemulti3@63580,20517 readhandle="h1422441682004"
;
;closing h1422441682004 on $zeof at 63580,20517
;
;readhandle=h1422441682001, zkey=READ|h1422441682001|::1;READ|h1422441682002|::1;READ|h1422441682003|::1
;connections("h1422441682000")=2
;connections("h1422441682001")=0
;connections("h1422441682002")=2
;connections("h1422441682003")=0
;connections("h1422441682004")=3
;connections("h1422441682004","$zeof")="63580,20517@waits=16"
;
;server+43^sockexamplemulti3@63580,20517 readhandle="h1422441682001"
;
;closing h1422441682001 on $zeof at 63580,20517
;
;readhandle=h1422441682002, zkey=READ|h1422441682002|::1;READ|h1422441682003|::1
;connections("h1422441682000")=2
;connections("h1422441682001")=1
;connections("h1422441682001","$zeof")="63580,20517@waits=17"
;connections("h1422441682002")=2
;connections("h1422441682003")=0
;connections("h1422441682004")=3
;connections("h1422441682004","$zeof")="63580,20517@waits=16"
;
;readhandle=, zkey=READ|h1422441682002|::1;READ|h1422441682003|::1
;connections("h1422441682000")=2
;connections("h1422441682001")=1
;connections("h1422441682001","$zeof")="63580,20517@waits=17"
;connections("h1422441682002")=2
;connections("h1422441682003")=0
;connections("h1422441682004")=3
;connections("h1422441682004","$zeof")="63580,20517@waits=16"
;
;server ends at JAN 28, 2015 05:41:57
;connections("h1422441682000")=2
;connections("h1422441682001")=1
;connections("h1422441682001","$zeof")="63580,20517@waits=17"
;connections("h1422441682002")=2
;connections("h1422441682003")=0
;connections("h1422441682004")=3
;connections("h1422441682004","$zeof")="63580,20517@waits=16"
;listensock("h1422441682002","read")="Partial"
;listensock("h1422441682003","read")="Partial"
;
;Client stderr:
;
;Client stdout:
;
;client ends at JAN 28, 2015 05:41:57
;connectionsock("client1","read")="Full"
;connectionsock("client1","y")="GT.M V6.2-001 Linux x86_64 server 24675 started at JAN 28, 2015 05:41:22"
;connectionsock("client1","zkey")=""
;connectionsock("client2","read")="Full"
;connectionsock("client2","readafter")="Partial"
;connectionsock("client2","y")="GT."
;connectionsock("client2","zkey")="READ|client2|::1"
;connectionsock("client3","read")="Full"
;connectionsock("client3","y")="GT.M V6.2-001 Linux x86_64 server 24675 started at JAN 28, 2015 05:41:22"
;connectionsock("client3","zkey")="READ|client2|::1"
;connectionsock("client4","read")="Full"
;connectionsock("client4","readafter")="Partial"
;connectionsock("client4","y")="GT."
;connectionsock("client4","zkey")="READ|client2|::1;READ|client4|::1"
;connectionsock("client5","read")="Full"
;connectionsock("client5","y")="GT.M V6.2-001 Linux x86_64 server 24675 started at JAN 28, 2015 05:41:22"
;connectionsock("client5","zkey")="READ|client2|::1;READ|client4|::1";

sockexamplemulti3
	new client,err1,err2,line,out1,out2,server
	set err1=$text(+0)_"_"_$job_"_server.mje"
	set out1=$extract(err1,1,$length(err1)-1)_"o"
	set cmd="server(,,,):(ERROR="""_err1_""":OUTPUT="""_out1_""")"
	job @cmd
	set server=$zjob
	set err2=$text(+0)_"_"_$job_"_client.mje"
	set out2=$extract(err2,1,$length(err2)-1)_"o"
	set cmd="client(,,):(ERROR="""_err2_""":OUTPUT="""_out2_""")"
	job @cmd
	set client=$zjob
	for  quit:$zsigproc(server,0)&$zsigproc(client,0)  write "At ",$zdate($horolog,"MON DD, YYYY 24:60:SS")," waiting for server job ",server," & client job ",client,! hang 3
	do showfile(err1,"Server stderr:")
	do showfile(out1,"Server stdout:")
	do showfile(err2,"Client stderr:")
	do showfile(out2,"Client stdout:")
	quit

client(portno,connectnum,timeout)
	new connectionsock,delim,detach,host,i,report,sockdev,socksel,val,x,y,zkey
	set:'($data(portno)#10) portno=$piece($horolog,",",1)#63487+2048
	set:'($data(connectnum)#10) connectnum=5
	set:'($data(timeout)#10) timeout=7
	set delim=$char(10) ; line feed
	set detach=""
	set host="localhost"
	set sockdev="client"
	set val=""
	for i=1:1:connectnum do ; open multiple socket connections to the server
	. set socksel="client"_i
	. open sockdev:(connect=host_":"_portno_":TCP":delim=delim:attach=socksel:exception="do ioerr(0,"_portno_")":ioerror="t"):timeout:"socket"
	. do:'$test ioerr(1,portno)
	. kill x,y
	. use sockdev:exception="do ioerr(2,"""""",$device)"
	. read x:timeout ; discard empty line from write !,msg,! by server
	. if i#2 do  ; argumentless DO protects $TEST from changes due to the timeout on the READ
	. . read y:timeout ; read of write !,msg,! by server - y has msg
	. . write "OK.",! ; respond to server with "OK."
  	. . zshow "D":r1
	. else  do
	. . read y#3:timeout ; partial read of write !,msg,! by server - y has 3 characters of msg
	. set zkey=$zkey,connectionsock(socksel,"read")=$select($length(y):"Full",1:"Timeout")
	. set connectionsock(socksel,"zkey")=zkey,connectionsock(socksel,"y")=y
	set totalpartialread=$length(zkey,"READ")-1 ; find the number of sockets which have partially read data
	for i=1:1:totalpartialread set connectionsock($piece($piece(zkey,"READ",i+1),"|",2),"readafter")="Partial"
	hang timeout*connectnum		; allow server to finish
	use $p write !,"client ends at "_$zdate($horolog,"MON DD, YYYY 24:60:SS"),! zwrite connectionsock
	quit

; log error and terminate process
ioerr(code,port,opt)
	new msg,stack
	set msg=$text(ioerrs+$get(code))
	set ioerrio=$io,ioerrdev=$device,ioerrkey=$key,ioerrzstatus=$zstatus,ioerrzeof=$zeof
	use $principal ; since process will terminate, no need to save & restore $io
 	zshow "s":stack
	do log("At "_stack("S",2)_" "_$piece(msg,";",2)_" port "_$get(port)_$select($data(opt)#10:$piece(msg,";",3)_opt,1:""))
	use 0 zwrite ioerrio,ioerrdev,ioerrkey,ioerrzeof zshow "D"	
	zwrite:$data(connectionsock) connectionsock
	zwrite:$data(connections) connections
	zwrite:$data(connectionsd) connectionsd
	zwrite:$data(listensock) listensock
	zhalt code

; text of error messages
ioerrs	;unable to OPEN
	;connection failure; timeout=
	;device error

; display a message on $principal
log(str)
	new previo
	set previo=$IO
	use $principal
	write $zdate($horolog,"MON DD, YYYY 24:60:SS")," ",str,!
	use previo
	quit

; entry point for the server code
server(portno,msg,listenqueue,timeout)
	new childsock,delim,i,ip,key,parentsock,listensock,start,sockdev,totalpartialread,x,x1,zkey
	set start=$horolog
	set:'($data(portno)#10) portno=$piece(start,",",1)#63487+2048
	set:'($data(msg)#10) msg=$zversion_" server "_$job_" started at "_$zdate(start,"MON DD, YYYY 24:60:SS")
	set:'($data(listenqueue)#10) listenqueue=5
	set:'($data(timeout)#10) timeout=7
	set delim=$char(10) ; line feed
	set sockdev="server"
	use $principal write "** tcpserver is getting ready to start.**",!
	open sockdev:(listen=portno_":tcp":attach="server":delimiter=delim:exception="do ioerr(0,"_portno_")":ioerror="t")::"socket"
	use sockdev:exception="do ioerr(2,"""""""""""""_",$device)"
	write:listenqueue>1 /listen(listenqueue) ; if so specified, increase listen queue size from default of 1
	set key=$key
	do log("Open serverport "_portno_" successful, $key="_key)
	set parentsock=$piece(key,"|",2)
	set (i,waits,waittimeouts,readtimeouts,quit,totalpartialread)=0
	use sockdev:ioerror="f"
	for  do  quit:quit
	. write /wait(timeout)
	. if $test=0 if $I(waittimeouts)>10 set quit=1 quit
	. set key=$key,waits=$I(waits),waittimeouts=0
	. if $piece(key,"|",1)="CONNECT" do
	. . set i=$I(i)		; keep track of number of connections
	. . set childsock=$piece(key,"|",2),ip=$piece(key,"|",3)
	. . set connections(childsock)=0	; track which socket have read
	. . do log("Connection established, socket="_childsock_", ip="_ip)
	. . use sockdev:(socket=childsock)
	. . write !,msg,!
	. . if +$device do ioerr(2,"",$device)
	. else  if $piece(key,"|",1)="READ" do
	. . set childsock=$piece(key,"|",2),ip=$piece(key,"|",3)
	. . if connections(childsock)'=0 do	; WRITE /WAIT selected already read socket so select another
	. . . set zkey=$zkey,numzkeyread=$length(zkey,"READ")-1,quitfound=0
	. . . for j=1:1:numzkeyread do  quit:quitfound
	. . . . set readhandle=$piece($piece(zkey,":",j),"|",2)
	. . . . set previo=$io u 0 w !,"readhandle="_readhandle_", zkey="_zkey,! zwrite connections use previo
	. . . . if readhandle="" set quitfound=j quit
	. . . . if connections(readhandle)=0 set quitfound=j quit
	. . . if (quitfound=0)!(readhandle="") quit	;no connection unread partially
	. . else  set readhandle=childsock	; no reads for childsock yet
	. . if readhandle="" set quit=1 quit
	. . set connections(readhandle)=$I(connections(readhandle))
	. . use 0 write !,$zpos,"@",$H," " zwrite readhandle zshow "d":connectionsd(readhandle,$H)
	. . use sockdev:socket=readhandle
	. . read x#2:timeout ; simulate a partial read - client sends three characters back but server reads only two
	. . if $zeof do
	. . . set connections(readhandle,"$zeof")=$H_"@waits="_waits
	. . . if $zkey[readhandle  use 0 write !,"unread data for "_readhandle_" on $zeof",!
	. . . close sockdev:socket=readhandle	; client closed device so close this side
	. . . use 0 write !,"closing "_readhandle_" on $zeof at "_$H,! use sockdev
	. . else  if $device["tion reset" do
	. . . set connections(readhandle,$H)=$device_"@waits="_waits
	. . . if $zkey[readhandle  use 0 write !,"unread data for "_readhandle_" on conn reset",!
	. . . close sockdev:socket=readhandle	; $zeof should have caught
	. . . use 0 write !,"closing "_readhandle_" on conn reset at "_$H,! use sockdev
	. . else  do
	. . . if +$device do ioerr(2,"",$device)
	. . . if $test=0 set readtimeouts(childsock)=$I(readtimeouts) quit
	. . . set finalresult="Message successfully read from client:"_childsock
	. . . set zkey=$zkey do log(finalresult_", x="_x_", zkey="_zkey)
	. else  if $key'="" do ioerr(2,"",$device)
	kill listensock
	set x1=zkey ; find the number of sockets which have partially read data
	if $zkey'=zkey do
	. new previo,zkey2 set previo=$io,zkey2=$zkey
	. use 0 w !,"zkey mismatch after loop",!
	. zwrite zkey,zkey2
	. use previo
	set totalpartialread=$length(x1,"READ")-1
	for i=1:1:totalpartialread do  
	. set sockhandlepar=$piece($piece(zkey,";",i),"|",2)
	. set listensock(sockhandlepar,"read")="Partial"
	. use sockdev:(detach=sockhandlepar) ; detach the socket with partially read data
	. set jobprocessparam="handlepartialread(timeout):(output="""_sockhandlepar_".mjo"":input="_"""SOCKET:"_sockhandlepar_""""_")"
	. job @jobprocessparam ; fork a process to handle the detached socket
	use $principal
	write !,"server ends at "_$zdate($horolog,"MON DD, YYYY 24:60:SS"),! zwrite connections
	zwrite:totalpartialread>1 listensock
	close sockdev
 	zwrite r1
	quit

; read & output a file, then delete it
showfile(file,title)
	new previo
	set previo=$io
	write !,title,!
	open file for  use file read line quit:$zeof  use previo write line,!
	use previo close file:delete
	quit

; read the remaining data on the partially read socket
handlepartialread(timeout)
	use $principal:delim=$char(10) ; delimiters are not passed to the job
	read x ; odd-numbered client sockets sent three characters ("Ok.") but server reads two ("Ok"). x has the remaining ".".
	write x,!
	quit
