; Verify ACID properties of a backed up database
; This program is part of the exercises in the YottaDB Acculturation Workshop at
;   https://docs.yottadb.com/AcculturationGuide/acculturation.html and its use is discussed there.
; For the sake of simplicity, this program does no error handling.
xyzVerifyM
	; Initialization
	set (tCrab,tDelta,tHorse)=0
	; Iterate over nodes in database and verify ACID properties
	; find next nodes; if last node of all three global variables reached without any errors, quit signaling success
	for  set tCrab=$order(^Crab(tCrab)),tDelta=$order(^Delta(tDelta)),tHorse=$order(^Horse(tHorse)) quit:'$length(tCrab)&'$length(tDelta)&'$length(tHorse)  do
	. ; if time stamps of next node of each global variable don't match, quit signaling failure
	. if tDelta'=tCrab!(tCrab'=tHorse) write "ACID fail: tDelta=",tDelta,"; tCrab=",tCrab,"; tHorse=",tHorse,! zhalt 1
	. ; if values at next nodes of ^Crab() and ^Horse() don't match, quit signaling failure
	. if ^Crab(tCrab)+^Horse(tHorse) write "ACID fail: ^Crab(",tCrab,")=",^Crab(tCrab),"; ^Horse(",tHorse,")=",^Horse(tHorse),! zhalt 2
	. ; if the value at next node of ^Horse() is not the sum of this and all preceding nodes of ^Delta(), quit signaling failure
	. if $increment(sDelta,^Delta(tDelta))'=^Horse(tHorse) write "ACID fail: Sum ^Delta(0:",tDelta,")=",sDelta,"; ^Horse(",tHorse,")=",^Horse(tHorse),! zhalt 3
	write "ACID test pass",!
	quit
