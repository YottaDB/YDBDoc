; Update a multi-region database using ACID transactions
; This program is part of the exercises in the YottaDB Acculturation Workshop at
;   https://docs.yottadb.com/AcculturationGuide/acculturation.html and its use is discussed there.
; For the sake of simplicity, this program does no error handling.
xyzTransM
	use $principal:(ctrap=$char(3):nocenable:exception="halt")         ; terminate on Ctrl-C
	; loop until stopped, setting variables in a transaction that spans multiple regions
	for  do				; loop for ever
	.  tstart ()			; a transaction that updates 3 regions
	.    set time=$zut		; $zut is microseconds since Jan 1, 1970 UTC
	.    set rand=$random(2**32)
	.    set ^Delta(time)=rand
	.    set ^Crab(time)=^Crab($order(^Crab(""),-1))-rand
	.    set ^Horse(time)=^Horse($order(^Horse(""),-1))+rand
	.  tcommit
	.  hang .5
	quit
