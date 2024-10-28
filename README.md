# Lucia-lang

[![Latest Version](https://img.shields.io/crates/v/lucia-lang.svg)](https://crates.io/crates/lucia-lang)
[![API Documentation](https://docs.rs/lucia-lang/badge.svg)](https://docs.rs/lucia-lang)

**A programming language inspired by Lua and Python.**

Lucia-lang is still in the early stages of development.

The `v0.1.0` is a very early prototype with a simple GC, but it has some bugs and safety issues.

The `v0.2.0` is a complete rewrite that uses [gc-arena](https://github.com/kyren/gc-arena) as GC and includes some code from [piccolo](https://github.com/kyren/piccolo).

## References

This project heavily references the following projects:

- [piccolo](https://github.com/kyren/piccolo)
- [oxc](https://github.com/oxc-project/oxc)
- [rustc](https://github.com/rust-lang/rust)

## License

<sup>
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
</sub>
