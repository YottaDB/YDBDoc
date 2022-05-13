local ydb = require('yottadb')

local t_crab = '0'
local t_delta = '0'
local t_horse = '0'
local s_delta = 0

-- Iterate over nodes and verify ACID properties
while(true)
do
    -- Find next node; if the last node of all three global variables is reached without any errors,
    -- quit loop to signal success
    t_crab = ydb.subscript_next('^Crab', {t_crab})
    t_delta = ydb.subscript_next('^Delta', {t_delta})
    t_horse = ydb.subscript_next('^Horse', {t_horse})

    if (t_crab == nil and t_delta == nil and t_horse == nil)
    then
	break
    end

    -- If the timestamps of the next nodes of each of the global variables don't match,
    -- terminate program with exit code 1 to signal timestamp mismatch failure
    if (t_delta~=t_crab or t_crab~=t_horse)
    then
	print(string.format('ACID fail: tDelta=%s tCrab=%s tHorse=%s\n', t_delta, t_crab, t_horse))
	os.exit(1)
    end

    local v_crab = tonumber(ydb.get('^Crab', {t_crab}))
    local v_delta = tonumber(ydb.get('^Delta', {t_delta}))
    local v_horse = tonumber(ydb.get('^Horse', {t_horse}))

    -- If the values at the next node of ^Crab() and ^Horse() don't match
    -- terminate the program with exit code 2 to signal value mismatch failure
    -- In Lua, 0 evaluates to true thus explicit value check is needed
    if (v_crab + v_horse ~= 0)
    then
	print(string.format('ACID fail: ^Crab(%s)=%d; ^Horse(%s)=%d\n', t_crab, v_crab, t_horse, v_horse))
	os.exit(2)
    end

    -- If the value at the next node of ^Horse() is not the sum of this and all preceding
    -- nodes of ^Delta(), terminate program with exit code 3 to signal sum mismatch failure
    s_delta = s_delta + v_delta
    if (s_delta ~= v_horse)
    then
       print(string.format('ACID fail: Sum ^Delta(0:%s)=%d; ^Horse(%s)=%d\n', t_delta, v_delta, t_horse, v_horse))
       os.exit(3)
    end
end

print('ACID test pass\n')
os.exit(0)