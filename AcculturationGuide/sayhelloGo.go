package main

import "lang.yottadb.com/go/yottadb/v2"

func main() {
	defer yottadb.Shutdown(yottadb.MustInit())
	conn := yottadb.NewConn()

	// Store unicode greeting into node ^hello("Go")
	greeting := conn.Node("^hello", "Go")
	greeting.Set("สวัสดีชาวโลก") // Hello world in Thai
}
