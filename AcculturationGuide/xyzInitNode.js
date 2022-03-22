const ydb = require('nodem').Ydb();

ydb.open();
ydb.kill('^Crab');
ydb.kill('^Delta');
ydb.kill('^Horse');
ydb.set('^Crab', 0, '0');
ydb.set('^Horse', 0, '0');
ydb.close();
