const ydb = require('nodem').Ydb();

ydb.open();
ydb.set('^hello', 'Node.js', 'مرحبا بالعالم');
ydb.close();
