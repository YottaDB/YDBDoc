// Verify ACID properties of a backed-up database
const ydb = require('nodem').Ydb();

ydb.open();

let tCrab = '0';
let tDelta = '0';
let tHorse = '0';
let sDelta = 0;

// Iterate over nodes and verify ACID properties
while(true){
    /* Find next node; if the last node of all three global variables is reached without any errors
    quit loop to signal success */
    tCrab = ydb.order('^Crab', tCrab);
    tDelta = ydb.order('^Delta', tDelta);
    tHorse = ydb.order('^Horse', tHorse);
    let Crab = ydb.get('^Crab', tCrab);
    let Delta = ydb.get('^Delta', tDelta);
    let Horse = ydb.get('^Horse', tHorse);

    if (!tCrab && !tDelta && !tHorse){
	break;
    }

    /* If the timestamps of the next nodes of each of the global variable don't match,
    terminate program with exit code 1 to signal timestamp mismatch failure*/
    if (tDelta != tCrab || tCrab != tHorse){
	console.log(`ACID fail: tDelta=${tDelta} tCrab=${tCrab} tHorse=${tHorse}\n`);
	process.exit(1);
    }

    /* If the values at next node of ^Crab and ^Horse don't match,
    terminate program with exit code 2 to signal value mismatch failure*/
    if (Crab + Horse){
	console.log(`ACID fail: ^Crab(${tCrab})=${Crab}; ^Horse(${tHorse})=${Horse}\n`);
	process.exit(2);
    }
    /* If the value at the next node of ^Horse is not the sum of this and all preceding
    nodes of ^Delta, terminate program with exit code 3 to signal sum mismatch failure*/
    sDelta += Delta;
    if (sDelta != Horse){
	console.log(`ACID fail: Sum ^Delta(0:${tDelta})=${sDelta}; ^Horse(${tHorse})=${Horse}\n`);
	process.exit(3);
    }
    console.log("ACID test pass");
    process.exit(0);
}
ydb.close();
