#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         racket/runtime-path)

(provide (all-defined-out))

(module+ public
  (provide
   (rename-out [MDB_env-pointer?    env?]
               [MDB_txn-pointer?    txn?]
               [MDB_cursor-pointer? cursor?]

               [MDB_stat?                  stat?]
               [MDB_stat-ms_psize          stat-psize]
               [MDB_stat-ms_depth          stat-depth]
               [MDB_stat-ms_branch_pages   stat-branch-pages]
               [MDB_stat-ms_leaf_pages     stat-leaf-pages]
               [MDB_stat-ms_overflow_pages stat-overflow-pages]
               [MDB_stat-ms_entries        stat-entries]

               [MDB_envinfo?              envinfo?]
               [MDB_envinfo-me_mapaddr    envinfo-mapaddr]
               [MDB_envinfo-me_mapsize    envinfo-mapsize]
               [MDB_envinfo-me_last_pgno  envinfo-last-pgno]
               [MDB_envinfo-me_last_txnid envinfo-last-txnid]
               [MDB_envinfo-me_maxreaders envinfo-maxreaders]
               [MDB_envinfo-me_numreaders envinfo-numreaders])))

;; Based on https://github.com/jbclements/RSound/blob/master/rsound/private/s16vector-add.rkt
(define-runtime-path here "libs")

(define-ffi-definer deflmdb (ffi-lib
                             (build-path here
                                         (system-library-subpath #f)
                                         "lmdb")))

(define-cpointer-type _MDB_env-pointer)
(define-cpointer-type _MDB_txn-pointer)
(define-cpointer-type _MDB_cursor-pointer)
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

(define-cstruct _MDB_envinfo ([me_mapaddr    _pointer]
                              [me_mapsize    _size]
                              [me_last_pgno  _size]
                              [me_last_txnid _size]
                              [me_maxreaders _uint]
                              [me_numreaders _uint]))

;; The "MDB_" prefixes have been stripped off of the following constants since,
;; unlike C, we don't have to export constant integers with globally unique
;; names. Racket FFI uses symbols for bitmasks and enums instead.
(define _mdb_env_flags
  (_bitmask `(
              FIXEDMAP   = #x1
              NOSUBDIR   = #x4000
              RDONLY     = #x20000
              WRITEMAP   = #x80000
              NOMETASYNC = #x40000
              NOSYNC     = #x10000
              MAPASYNC   = #x100000
              NOTLS      = #x200000
              NOLOCK     = #x400000
              NORDAHEAD  = #x800000
              NOMEMINIT  = #x1000000
              )
            _uint))

(define _mdb_db_flags
  (_bitmask '(
              REVERSEKEY = #x2
              DUPSORT    = #x4
              INTEGERKEY = #x8
              DUPFIXED   = #x10
              INTEGERDUP = #x20
              REVERSEDUP = #x40
              CREATE     = #x40000
              )
            _uint))

(define _mdb_write_flags
  (_bitmask '(
              NOOVERWRITE = #x10
              NODUPDATA   = #x20
              CURRENT     = #x40
              ;; Unsupported since we can't cleanly grant access to raw memory
              ;; locations or mutate them.
              ;;
              ;; RESERVE  = #x10000
              APPEND      = #x20000
              APPENDDUP   = #x40000
              ;; Unsupported
              ;; MULTIPLE    = #x80000
              )
            _uint))

(define _mdb_copy_flags
  (_bitmask '(CP_COMPACT = #x1)))

(define _MDB_msg_func (_fun _string _pointer -> _int))

(define _MDB_cursor_op
  (_enum '(FIRST
           FIRST_DUP
           GET_BOTH
           GET_BOTH_RANGE
           GET_CURRENT
           GET_MULTIPLE
           LAST
           LAST_DUP
           NEXT
           NEXT_DUP
           NEXT_MULTIPLE
           NEXT_NODUP
           PREV
           PREV_DUP
           PREV_NODUP
           SET
           SET_KEY
           SET_RANGE)))
