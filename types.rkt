#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         racket/runtime-path)

(provide (all-defined-out))

;; Based on https://github.com/jbclements/RSound/blob/master/rsound/private/s16vector-add.rkt
(define-runtime-path here "libs")

(define-ffi-definer deflmdb (ffi-lib
                             (build-path here
                                         (system-library-subpath #f)
                                         "lmdb")))

(define _MDB_env-pointer (_cpointer 'MDB_env))
(define _MDB_txn-pointer (_cpointer 'MDB_txn))
(define _MDB_txn-pointer/null (_cpointer/null 'MDB_txn))
(define _MDB_cursor-pointer (_cpointer 'MDB_cursor))
(define _MDB_dbi _uint)
(define _mdb_mode_t _int)

(define-cstruct _MDB_val ([mv_size _size]
                          [mv_data _pointer]))

(define-cstruct _MDB_stat ([ms_psize          _uint]
                           [ms_depth          _uint]
                           [ms_branch_pages   _size]
                           [ms_leaf_pages     _size]
                           [ms_overflow_pages _size]
                           [ms_entries        _size]))

(define _mdb_env_flags
  (_bitmask `(
              MDB_FIXEDMAP   = #x1
              MDB_NOSUBDIR   = #x4000
              MDB_RDONLY     = #x20000
              MDB_WRITEMAP   = #x80000
              MDB_NOMETASYNC = #x40000
              MDB_NOSYNC     = #x10000
              MDB_MAPASYNC   = #x100000
              MDB_NOTLS      = #x200000
              MDB_NOLOCK     = #x400000
              MDB_NORDAHEAD  = #x800000
              MDB_NOMEMINIT  = #x1000000
              )
            _uint))

(define _mdb_db_flags
  (_bitmask '(
              MDB_REVERSEKEY = #x2
              MDB_DUPSORT    = #x4
              MDB_INTEGERKEY = #x8
              MDB_DUPFIXED   = #x10
              MDB_INTEGERDUP = #x20
              MDB_REVERSEDUP = #x40
              MDB_CREATE     = #x40000
              )
            _uint))

(define _mdb_write_flags
  (_bitmask '(
              MDB_NOOVERWRITE = #x10
              MDB_NODUPDATA   = #x20
              MDB_CURRENT     = #x40
              ;; Unsupported since we can't cleanly grant access to raw memory
              ;; locations or mutate them.
              ;;
              ;; MDB_RESERVE  = #x10000
              MDB_APPEND      = #x20000
              MDB_APPENDDUP   = #x40000
              ;; Unsupported
              ;; MDB_MULTIPLE    = #x80000
              )
            _uint))

(define _MDB_cursor_op
  (_enum '(MDB_FIRST
           MDB_FIRST_DUP
           MDB_GET_BOTH
           MDB_GET_BOTH_RANGE
           MDB_GET_CURRENT
           MDB_GET_MULTIPLE
           MDB_LAST
           MDB_LAST_DUP
           MDB_NEXT
           MDB_NEXT_DUP
           MDB_NEXT_MULTIPLE
           MDB_NEXT_NODUP
           MDB_PREV
           MDB_PREV_DUP
           MDB_PREV_NODUP
           MDB_SET
           MDB_SET_KEY
           MDB_SET_RANGE)))
