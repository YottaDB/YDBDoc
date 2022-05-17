const ydb = require('nodem').Ydb();

ydb.open();

// remove existing global variable trees
ydb.kill('^Crab');
ydb.kill('^Delta');
ydb.kill('^Horse');

// set initial values for ^Crab and ^Horse
ydb.set('^Crab', 0, '0');
ydb.set('^Horse', 0, '0');
ydb.close();
