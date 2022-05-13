local ydb = require('yottadb')

math.randomseed(ydb.get('$zut'))

function xyzTrans()

	 local rand_int = math.random(0,2^32)
	 local curr_time = tostring(ydb.get('$zut'))

	 local crab = ydb.key('^Crab')
	 local delta = ydb.key('^Delta')
	 local horse = ydb.key('^Horse')

	 delta(curr_time):set(tostring(rand_int))
	 crab(curr_time):set(tostring(tonumber(crab(crab:subscript_previous()).value) - rand_int))
	 horse(curr_time):set(tostring(tonumber(horse(horse:subscript_previous()).value) + rand_int))
end

while(true)
do
	ydb.tp(xyzTrans)
	os.execute('sleep 0.5')
end