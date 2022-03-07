#![allow(non_snake_case)]

use yottadb::simple_api::Key;
use yottadb::craw::YDB_NOTTP;

fn main() {
    let err_buffer = Vec::new();
    let mut hello = Key::new("^hello", &["Rust"]);
    hello.set_st(YDB_NOTTP, err_buffer, "こんにちは世界".as_bytes()).unwrap();
}
