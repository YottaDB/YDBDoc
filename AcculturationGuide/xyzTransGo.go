// Update a multi-region database using ACID transactions
// This program is part of the exercises in the YottaDB Acculturation Workshop at
//   https://docs.yottadb.com/AcculturationGuide/acculturation.html and its use is discussed there.
// For the sake of simplicity, this program does no error handling.
package main

import (
	"math/rand"
	"strconv"
	"time"
	"lang.yottadb.com/go/yottadb"
)

func main() {
	var currtime, lasttime []string
	var lastvali, xyzrandi int
	var lastvals, xyzrands string

	defer yottadb.Exit()
	currtime = make([]string, 1)
	lasttime = make([]string, 1)
	for ; ; {	// loop for ever
		_ = yottadb.TpE(yottadb.NOTTP, nil,	// a transaction that updates 3 regions
			func(tptoken uint64, errstr *yottadb.BufferT) int32 {
				currtime[0] = strconv.Itoa(int(time.Now().UnixNano()/1e3)) // currtime[0] is string of microsecnds since Jan 1, 1970 UTC
				xyzrandi = int(rand.Int31())
				xyzrands = strconv.Itoa(xyzrandi)
				_ = yottadb.SetValE(tptoken, nil, xyzrands, "^Delta", currtime)
				lasttime[0], _ = yottadb.SubPrevE(tptoken, nil, "^Crab", []string{""})
				lastvals, _ = yottadb.ValE(tptoken, nil, "^Crab", lasttime)
				lastvali, _ = strconv.Atoi(lastvals)
				_ = yottadb.SetValE(tptoken, nil, strconv.Itoa(lastvali-xyzrandi), "^Crab", currtime)
				lasttime[0], _ = yottadb.SubPrevE(tptoken, nil, "^Horse", []string{""})
				lastvals, _ = yottadb.ValE(tptoken, nil, "^Horse", lasttime)
				lastvali, _ = strconv.Atoi(lastvals)
				_ = yottadb.SetValE(tptoken, nil, strconv.Itoa(lastvali+xyzrandi), "^Horse", currtime)
				return yottadb.YDB_OK
			}, "BATCH", nil)
		time.Sleep(500 * time.Millisecond)
	}
}
