# Racket LMDB Wrapper

This module is a direct port of the [LMDB API](http://www.lmdb.tech/doc), with the following exceptions:

1. All keys and data are passed around as [byte strings](https://docs.racket-lang.org/reference/bytestrings.html), not `MDB_val`s, or `#f` in place of `NULL`.
   Note that converting between `MDB_val` and byte strings incurs a performance overhead.
2. All functions that return a status, unless otherwise noted, now throw failures as `exn:mdb` structs.
3. In the cases where a function takes a single out parameter and returns a status (e.g. `mdb_dbi_open`, `mdb_cursor_count`), that out parameter is returned instead since failures are thrown.
4. `mdb_get` returns the value that was read or `#f` if it was not found.
   (Any error other than `MDB_NOTFOUND` will be raised.)
5. `mdb_cursor_get` takes two mutable boxes for the key and data.
   These boxes will be modified to contain the returned key and data if applicable.
   `#t` is returned on success and `#f` is returned if the status is `MDB_NOTFOUND`.
   (Any error other than `MDB_NOTFOUND` will be raised.)
6. Racket booleans are used for the int parameters of `mdb_drop` and `mdb_env_sync`.

As a very direct port, this library gives you plenty of leeway to shoot yourself in the foot.

## Limitations

* The `MDB_RESERVE` and `MDB_MULTIPLE` write flags are not supported.
* The `mdb_set_*` functions haven't been exposed.
* Performance:
  * This library copies a lot of memory around when converting to and from byte strings.
  * Memory mapping, as used by LMDB, involves cache misses which block the entire OS thread.
    This means that the entire Racket place can be blocked, since Racket uses green threads.
    (i.e. Code in another Racket [thread](https://docs.racket-lang.org/reference/threads.html) can be blocked by LMDB operations if they are in the same [place](https://docs.racket-lang.org/reference/places.html)!)

## Usage Notes

* You'll likely want to open the database with `MDB_NOTLS` since all Racket threads within the same place are run on the same OS thread.
  If you don't, LMDB will use thread-local storage to track transactions and you won't be able to perform multiple read-only transactions within the same place.
* This library does no extra bookkeeping, so as with regular LMDB, it's up to you to commit, abort or renew transactions.

## Bundled Libraries

This package bundles LMDB dynamic libraries for various platforms which were sourced from these locations:

* Windows 64-bit: https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-lmdb-0.9.29-1-any.pkg.tar.zst (MSYS2)
* Linux 64-bit: https://mirror.pkgbuild.com/extra/os/x86_64/lmdb-0.9.29-1-x86_64.pkg.tar.zst (Arch Linux)

## License

LMDB is released under the [OpenLDAP license](libs/lmdb-license.txt).
The wrapper itself is licensed under the [MIT license](LICENSE.txt).
