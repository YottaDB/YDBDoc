//! An example of `yottadb` using the `context_api`.

use yottadb::{Context, KeyContext as Key, YDBError};

fn main() -> Result<(), YDBError> {
    let ctx = Context::new();

    let hello = Key::new(&ctx, "^hello", &["Rust"]);
    hello.set("ハローワールド")?;
    assert_eq!(hello.get()?, "ハローワールド".as_bytes());

    Ok(())
}
