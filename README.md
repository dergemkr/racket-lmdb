# Racket LMDB Wrapper

**Version 0.2: Please note future API-breaking changes are possible!**

This module is a direct port of the [LMDB API](http://www.lmdb.tech/doc), with the following exceptions:

1. The names have been adjusted to Scheme standards.
   See [below](#naming).
2. All keys and data are passed around as [byte strings](https://docs.racket-lang.org/reference/bytestrings.html), not `MDB_val`s, or `#f` in place of `NULL`.
3. All functions that return a status, unless otherwise noted, now throw failures as `exn:mdb` structs.
4. In the cases where a function takes a single out parameter and returns a status (e.g. `dbi-open`, `cursor-count`), that out parameter is returned instead since failures are thrown.
5. `mdb-get` returns the value that was read or `#f` if it was not found.
   Any error other than `MDB_NOTFOUND` will be raised.
6. `cursor-get` takes two mutable boxes for the key and data.
   These boxes will be modified to contain the returned key and data if applicable.
   `#t` is returned on success and `#f` is returned if the status is `MDB_NOTFOUND`.
   Any error other than `MDB_NOTFOUND` will be raised.
7. Racket booleans are used for the int parameters of `dbi-drop`, `env-set-flags`, and `env-sync`.
8. All the `mdb_env_copy*` functions have been condensed down to `env-copy`, which takes flags as an optional argument and dispatches to the `copy` or `copyfd` version based on the type of the second argument.
9. Two forms, `with-txn` and `with-cursor`, are provided to assist with resource management.

As a very direct port, this library gives you plenty of leeway to shoot yourself in the foot.

## Naming

The names in this library have been updated to fit a bit better into the Racket ecosystem.
Underscores in procedure names have been replaced with dashes and in most places, the `MDB_`/`mdb_` prefix have been stripped off, except where the stripped names wouldn't be informative enough on their own.

Examples:

* The `mdb_env_create` procedure is now just `env-create`.
* The `MDB_FIRST` cursor op is now just the symbol `'FIRST`.
* The `MDB_DUPSORT` database flag is now just the symbol `'DUPSORT`.

Exceptions:

* `mdb_drop` has been renamed to `dbi-drop`.
  This aligns it with the other `dbi-` procedures and saves us from conflicting with Racket's built-in `drop` that operates on lists.
* `mdb_version` has been renamed to `mdb-version` so that it doesn't conflict with Racket's `version` function.
* `mdb_strerror` has been renamed to `mdb-strerror` because `strerror` is too general on its own.
* All error constants still have the `MDB_` prefix.

## Limitations

* Unsupported Features:
   * The `MDB_RESERVE` and `MDB_MULTIPLE` write flags are not supported.
   * The following functions have not been exposed:
     * `mdb_env_set_userctx`
     * `mdb_env_get_userctx`
     * `mdb_env_set_assert`
     * `mdb_set_compare`
     * `mdb_set_dupsort`
     * `mdb_set_relfunc`
     * `mdb_set_relctx`
     * `mdb_cmp`
     * `mdb_dcmp`
* Performance:
  * This library copies a lot of memory around when converting to and from byte strings.
  * Memory mapping, as used by LMDB, involves cache misses which block the entire OS thread.
    This means that the entire Racket place can be blocked, since Racket uses green threads.
    (i.e. Code in another Racket [thread](https://docs.racket-lang.org/reference/threads.html) can be blocked by LMDB operations if they are in the same [place](https://docs.racket-lang.org/reference/places.html)!)

## Usage Notes

* You'll likely want to open the database with `MDB_NOTLS` since all Racket threads within the same place are run on the same OS thread.
  If you don't, LMDB will use thread-local storage to track transactions and you won't be able to perform multiple read-only transactions within the same place.
* This library does no extra bookkeeping, so as with regular LMDB, it's up to you to clean up what you create, be they environments, transactions, or cursors.
  The `with-txn` and `with-cursor` forms can assist in cases.

## Bundled Libraries

This package bundles LMDB dynamic libraries for various platforms which were sourced from these locations:

* Windows 64-bit: https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-lmdb-0.9.29-1-any.pkg.tar.zst (MSYS2)
* Linux 64-bit: https://mirror.pkgbuild.com/extra/os/x86_64/lmdb-0.9.29-1-x86_64.pkg.tar.zst (Arch Linux)

## License

LMDB is released under the [OpenLDAP license](libs/lmdb-license.txt).
The wrapper itself is licensed under the [MIT license](LICENSE.txt).
