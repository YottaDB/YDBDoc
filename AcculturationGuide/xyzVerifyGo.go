// Verify ACID properties of a backed up database
// This program is part of the exercises in the YottaDB Acculturation Workshop at
// https://docs.yottadb.com/AcculturationGuide/acculturation.html and its use is discussed there.
// For the sake of simplicity, this program does no error handling.
package main

import(
	"fmt"
	"os"
	"strconv"
	"lang.yottadb.com/go/yottadb")

func main() {
	var tCrab, tDelta, tHorse []string // timestamps (subscripts) of ^Crab(), ^Delta(), and ^Horse() nodes
	var errtCrab, errtDelta, errtHorse, sDelta, tmp1, tmp2, tmp3 int
	var err error
	var vCrab, vDelta, vHorse string // values of ^Crab(), ^Delta(), and ^Horse() nodes

	// Initialization
	tCrab, tDelta, tHorse = make([]string, 1), make([]string, 1), make([]string, 1)
	tCrab[0], tDelta[0], tHorse[0] = "0", "0","0"
	sDelta = 0
	defer yottadb.Exit()
	// Iterate over nodes in database and verify ACID properties
	for ; ; {
		// find next nodes; if last node of all three global variables reached without any errors, quit signaling success
		tCrab[0], err = yottadb.SubNextE(yottadb.NOTTP, nil, "^Crab", tCrab)
		if nil != err { errtCrab = yottadb.ErrorCode(err) }
		tDelta[0], err = yottadb.SubNextE(yottadb.NOTTP, nil, "^Delta", tDelta)
		if nil != err { errtDelta = yottadb.ErrorCode(err) }
		tHorse[0], err = yottadb.SubNextE(yottadb.NOTTP, nil, "^Horse", tHorse)
		if nil != err { errtHorse = yottadb.ErrorCode(err) }
		if errtCrab == yottadb.YDB_ERR_NODEEND && errtDelta == yottadb.YDB_ERR_NODEEND && errtHorse == yottadb.YDB_ERR_NODEEND { break }
		// Confirm that timestamps (subscripts) of ^Crab(), ^Delta(), and ^Horse() match; if not exit with failure code
		if tDelta[0] != tCrab[0] || tCrab[0] != tHorse[0] {
			fmt.Println("ACID fail: tDelta=", tDelta[0], "; tCrab=", tCrab[0], "; tHorse=", tHorse[0])
			yottadb.Exit()
			os.Exit(1)
		}
		// Confirm that sum of ^Crab() and ^Horse() nodes is zero; if not exit with failure code
		vCrab, _ = yottadb.ValE(yottadb.NOTTP, nil, "^Crab", tCrab)
		vHorse, _ = yottadb.ValE(yottadb.NOTTP, nil, "^Horse", tHorse)
		tmp1, _ = strconv.Atoi(vCrab)
		tmp2, _ = strconv.Atoi(vHorse)
		if 0 != tmp1 + tmp2 {
			fmt.Println("ACID fail: ^Crab(", tCrab[0], ")=", vCrab, "; ^Horse(", tHorse[0], ")=", vHorse)
			yottadb.Exit()
			os.Exit(2)
		}
		// Confirm that value of ^Horse() node is equal to the sum of ^Delta() nodes from 0 through this timestamp
		vDelta, _ = yottadb.ValE(yottadb.NOTTP, nil, "^Delta", tDelta)
		tmp3, _ = strconv.Atoi(vDelta)
		sDelta += tmp3
		if sDelta != tmp2 {
			fmt.Println("ACID fail: Sum ^Delta(0:", tDelta[0], ")=", sDelta, "^Horse(", tHorse[0], ")=", vHorse)
			yottadb.Exit()
			os.Exit(3)
		}
	}
	fmt.Println("ACID test pass")
}
