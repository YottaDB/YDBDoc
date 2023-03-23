;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Example M program to demonstrate how to pass a listening TCP socket to a
; child process through a local socket.

; The `passTCPSocket` routine runs as the parent process. It will spawn and
; coordinate socket operations between two child processes. One child process
; will act as a basic TCP client, while the other will act as basic TCP server.

; The `passTCPSocket` routine will first spawn the TCP client process. Then it
; will open a TCP socket to listen for TCP requests from the client process.
; However, rather than retaining this listening TCP socket for itself,
; `passTCPSocket` will pass this TCP socket to another child process via a
; *local* socket. This second child process acts as the TCP "server".

passTCPSocket ;
	new $etrap set $etrap="goto handleError"

	write "Starting TCP socket demo...",!

	write "Removing old data...",!
	zsystem "rm client.out server.out"
	kill ^tcpOutput             ; Clean up any pre-existing global nodes from previous runs

	write "Parent process pid = ",$job,!
	job client:(output="client.out")	 ; Spawn a new YDB process to act as a TCP client
	job server:(output="server.out")         ; Spawn job to receive the detached TCP socket

	; Open a new socket on `tcpSocket` that will:
	;   1. Listen on TCP port 8080
	;   2. Be referenced by the handle name "handle1"
	;   3. Terminate READ operations using a delimiter consisting of the ASCII characters `\r\n`, i.e. carriage return + newline.
	;   4. Timeout after 15 seconds if it cannot be opened
	open "tcpSocket":(listen="8080:TCP":delim=$char(13,10):attach="handle1"):15:"socket"
	use "tcpSocket":(detach="handle1")      ; Detach the listening socket
	; Open a local socket to receive the detached TCP socket
	open "localSocket":(connect="socket2:local")::"socket"
	use "localSocket"                    ; Switch the I/O device to the new local socket
	; Pass the detached socket to the child process through the new local socket
	write /pass(,,"handle1")
	; Now the child process owns the listening TCP socket
	close "localSocket"
	use $principal

	; Wait for child and server to communicate
	; There are 6 lines in the data sent in server, and thus the 6 (making sure we got all the data)
	for  quit:$data(^tcpOutput(6))  hang .01
	write "Socket demo complete.",!
	write "----------",!
	write "My devices",!
	zshow "d"
	write "----------",!
	write "Contents of `client.out` and `server.out` for results:",!
	write !
	write "client.out",!
	write "----------",!
	zsystem "cat client.out"
	write !
	write "server.out",!
	write "----------",!
	zsystem "cat server.out"
	quit

; Simple routine that acts as a rudimentary TCP server. Spawned as a child of the `passTCPSocket` routine.
server   ;
	new handle,input
	write "Server process pid = ",$job,!
	new $etrap set $etrap="goto handleError"
	; Open a new socket on `socket` that will:
	;   1. Listen on a local socket named "socket2"
	;   2. Be referenced by the handle name "handle2"
	open "socket":(listen="socket2:local")::"socket"
	use "socket"                   ; Switch the I/O device to the new local socket
	write /wait
	write /accept(.handle,,,"handle1")  ; Accept the listening TCP socket from parent process
	; Switch the I/O device to the received TCP socket
	use "socket":(attach=handle)
	; Set chset to "M" mode to read TCP input stream as unencoded bytes instead of UTF-8 characters
	use "socket":(delim=$C(13,10):chset="M")
	write /listen(1)                     ; Set listen queue size to 1
	write /wait(10)                      ; Wait for client to connect for up to 10 seconds
	read input:10                       ; Read input data from TCP socket
	; Issue a dummy HTTP response
	write "HTTP/1.1 200 OK"_$C(13,10)
	write "Date: Mon, 27 Jul 2009 12:28:53 GMT"_$C(13,10)
	write "Server: YottaDB"_$C(13,10)
	write "Last-Modified: Wed, 22 Jul 2009 19:15:56 GMT"_$C(13,10)
	write "Content-Length: 0"_$C(13,10)
	write "Connection: Closed"_$C(13,10)
	use $principal                      ; Reset the current I/O device from the socket to print results to terminal
	write !,"Data received from client:",!
	write input,!
	write !,"Devices before closure:",!
	zshow "d"
	; Close the opened sockets
	write !,"Closing Sockets",!
	close "socket":(socket=handle)
	close "socket"
	write "Devices after closure:",!
	zshow "d"
	quit

; Simple routine that acts as a rudimentary TCP client. Spawned as a child of the `passTCPSocket` routine.
client
	new i
	write "Client process pid = ",$job,!
	new $etrap set $etrap="goto handleError"
	; Open a new socket on `tcpSocket` that will:
	;   1. Connect to TCP port 8080 on IP address 127.0.0.1
	;   2. Be referenced by the handle name "client1"
	open "tcpSocket":(connect="[127.0.0.1]:8080:TCP":attach="client1")::"socket"
	; Set the new socket as the current I/O device, using a READ delimiter consisting
	; of the ASCII characters `\r\n`, i.e. carriage return + newline.
	use "tcpSocket":(delim=$C(13,10):chset="M")
	; Write some test input to the socket, terminate it with the delimiter specified above,
	; and flush the socket buffer with `,!`
	write "TESTDATA"_$C(13,10),!
	; Read the test input from the socket into a global variable node (GVN) to persist it
	; The read will continue till the last line of the read, which is like a delimiter.
	for  read ^tcpOutput($incr(i))  quit:i>10  quit:^tcpOutput(i)="Connection: Closed"
	use $principal              ; Reset the current I/O device from the socket to print results to terminal
	zshow "d"
	close "tcpSocket"
	zwr ^tcpOutput              ; Write the data stored in the output node to the default device
	quit

handleError
	use $principal
	set $ecode=""
	write $zstatus
	quit
