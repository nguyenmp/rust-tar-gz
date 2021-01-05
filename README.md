# rust-tar-gz
rust tarball decompressor

Install rust from https://www.rust-lang.org/learn/get-started:

```
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

Build and run this project:

```
cargo build
```

Generate a test tarball:

```
tar -czvf archive.tar.gz src Cargo.lock Cargo.toml
```

Run the program:

```
cargo run $(pwd)/archive.tar.gz $(pwd)/output.tar
```

Note, the program will always terminate with "not implemented".
