package main

import (
	"lang.yottadb.com/go/yottadb"
)

func main() {
	defer yottadb.Exit()
	// remove existing global variable trees
	_ = yottadb.DeleteE(yottadb.NOTTP, nil, yottadb.YDB_DEL_TREE, "^Crab", nil)
	_ = yottadb.DeleteE(yottadb.NOTTP, nil, yottadb.YDB_DEL_TREE, "^Delta", nil)
	_ = yottadb.DeleteE(yottadb.NOTTP, nil, yottadb.YDB_DEL_TREE, "^Horse", nil)
	// set initial values for ^Crab and ^Horse
	_ = yottadb.SetValE(yottadb.NOTTP, nil, "0", "^Crab", []string{"0"})
	_ = yottadb.SetValE(yottadb.NOTTP, nil, "0", "^Horse", []string{"0"})
}
