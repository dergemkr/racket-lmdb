# Racket LMDB Wrapper

This module is a direct port of the [LMDB API](http://www.lmdb.tech/doc), with the following exceptions:

1. All functions that return a status, unless otherwise noted, now throw failures as `exn:mdb` structs.
2. In the cases where a function takes a single out parameter and returns a status (e.g. `mdb_dbi_open`, `mdb_cursor_count`), that out parameter is returned instead since failures are thrown.
3. `mdb_get` returns the value that was read or `#f` if it was not found.
   (Any error other than `MDB_NOTFOUND` will be raised.)
4. `mdb_cursor_get` takes two mutable boxes for the key and data.
   These boxes will be modified to contain the returned key and data if applicable.
   `#t` is returned on success and `#f` is returned if the status is `MDB_NOTFOUND`.
   (Any error other than `MDB_NOTFOUND` will be raised.)

## Limitations

* The `MDB_RESERVE` and `MDB_MULTIPLE` write flags are not supported.
* Performance:
  * This library copies a lot of memory around when converting to and from byte strings.
  * Memory mapping, as used by LMDB, involves cache misses which block the entire OS thread.
    This means that the entire Racket place can be blocked, since Racket uses green threads.
    (i.e. Code in another Racket [thread](https://docs.racket-lang.org/reference/threads.html) can be blocked by LMDB operations if they are in the same [place](https://docs.racket-lang.org/reference/places.html)!

## Bundled Libraries

This package bundles LMDB dynamic libraries for various platforms which were sourced from these locations:

* Windows 64-bit: https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-lmdb-0.9.29-1-any.pkg.tar.zst (MSYS2)
* Linux 64-bit: https://mirror.pkgbuild.com/extra/os/x86_64/lmdb-0.9.29-1-x86_64.pkg.tar.zst (Arch Linux)
