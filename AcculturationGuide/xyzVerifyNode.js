const ydb = require('nodem').Ydb();

ydb.open();

let tCrab = '0';
let tDelta = '0';
let tHorse = '0';
let sDelta = 0;

while(true){
    tCrab = ydb.order('^Crab', tCrab);
    tDelta = ydb.order('^Delta', tDelta);
    tHorse = ydb.order('^Horse', tHorse);
    let Crab = ydb.get('^Crab', tCrab);
    let Delta = ydb.get('^Delta', tDelta);
    let Horse = ydb.get('^Horse', tHorse);

    if (!tCrab && !tDelta && !tHorse){
	break;
    }
    if (tDelta != tCrab || tCrab != tHorse){
	console.log(`ACID fail: tDelta=${tDelta} tCrab=${tCrab} tHorse=${tHorse}\n`);
	process.exit(1);
    }
    if (Crab + Horse){
	console.log(`ACID fail: ^Crab(${tCrab})=${Crab}; ^Horse(${tHorse})=${Horse}\n`);
	process.exit(2);
    }
    sDelta += Delta;
    if (sDelta != Horse){
	console.log(`ACID fail: Sum ^Delta(0:${tDelta})=${sDelta}; ^Horse(${tHorse})=${Horse}\n`);
	process.exit(3);
    }
    console.log("ACID test pass");
    process.exit(0);
}
ydb.close();
