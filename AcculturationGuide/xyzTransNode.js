const ydb=require('nodem').Ydb();

ydb.open();

setInterval(() => {
    ydb.transaction(() => {
	let time = Date.now();
	let rand = Math.floor(Math.random() * Math.pow(2,32));
	ydb.set('^Delta', time, rand);
	ydb.set('^Crab', time, ydb.get('^Crab', ydb.previous('^Crab', '')) - rand);
	ydb.set('^Horse', time, ydb.get('^Horse', ydb.previous('^Horse', '')) + rand);
    }, {variables: ['*'], type: 'BATCH'});
}, 500);
