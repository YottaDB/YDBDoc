package main

import (
	"lang.yottadb.com/go/yottadb"
)

func main() {

	defer yottadb.Exit()
	_ = yottadb.SetValE(yottadb.NOTTP, nil, "สวัสดีชาวโลก", "^hello", []string{"Go"})
}
