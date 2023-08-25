#lang racket/base

;; The functions in this file are defined in the same order as they're listed in
;; the LMDB API documentation.

(require ffi/unsafe
         racket/string
         "errors.rkt"
         "types.rkt"
         "utils.rkt")

(module+ test
  (require rackunit))

(provide (except-out (all-defined-out)
                     check-status))

; TODO: Make sure we follow error string conventions
(define (check-status s)
  (unless (zero? s)
    (raise (exn:mdb (format "exn:mdb: ~a" (mdb_strerror s))
                    (current-continuation-marks)
                    s))))

(deflmdb mdb_version
  (_fun
   (major : (_ptr o _int))
   (minor : (_ptr o _int))
   (patch : (_ptr o _int))
   -> (readable : _string)
   -> (values major minor patch readable)))

(module+ test
  (test-case "mdb_version"
    (define-values (major minor patch readable) (mdb_version))
    ;; We can hard-code this since we bundle the library right now.
    (check-equal? major 0)
    (check-equal? minor 9)
    (check-equal? patch 29)
    (check-true (string-contains? readable "0.9.29"))))

(deflmdb mdb_strerror
  (_fun _int -> _string))

(deflmdb mdb_env_create
  (_fun (o : (_ptr o _MDB_env-pointer))
        -> (s : _int)
        -> (begin
             (check-status s)
             o)))

(deflmdb mdb_env_open
  (_fun _MDB_env-pointer
        _string
        _mdb_env_flags
        _mdb_mode_t
        -> (s : _int)
        -> (check-status s))
  #:wrap (lambda (raw)
           (lambda (env path flags mode)
             (raw env
                  (path->complete-path path)
                  flags
                  mode))))

(deflmdb mdb_env_close
  (_fun _MDB_env-pointer -> _void))

(deflmdb mdb_env_get_flags
  (_fun _MDB_env-pointer
        (f : (_ptr o _mdb_env_flags))
        -> (s : _int)
        -> (begin
             (check-status s)
             f)))

(deflmdb mdb_env_set_maxreaders
  (_fun _MDB_env-pointer
        _uint
        -> (s : _int)
        -> (check-status s)))

(deflmdb mdb_env_get_maxreaders
  (_fun _MDB_env-pointer
        (r : (_ptr o _uint))
        -> (s : _int)
        -> (begin
             (check-status s)
             r)))

(deflmdb mdb_env_set_maxdbs
  (_fun _MDB_env-pointer
        _MDB_dbi
        -> (s : _int)
        -> (check-status s)))

(deflmdb mdb_txn_begin
  (_fun _MDB_env-pointer
        _MDB_txn-pointer/null
        _mdb_env_flags
        (x : (_ptr o _MDB_txn-pointer))
        -> (s : _int)
        -> (begin
             (check-status s)
             x)))

(deflmdb mdb_txn_env
  (_fun _MDB_txn-pointer
        -> _MDB_env-pointer))

(deflmdb mdb_txn_id
  (_fun _MDB_txn-pointer
        -> _size))

(deflmdb mdb_txn_commit
  (_fun _MDB_txn-pointer
        -> (s : _int)
        -> (check-status s)))

(deflmdb mdb_txn_abort
  (_fun _MDB_txn-pointer -> _void))

(deflmdb mdb_txn_reset
  (_fun _MDB_txn-pointer -> _void))

(deflmdb mdb_txn_renew
  (_fun _MDB_txn-pointer
        -> (s : _int)
        -> (check-status s)))

(deflmdb mdb_dbi_open
  (_fun _MDB_txn-pointer
        _string
        _mdb_db_flags
        (d : (_ptr o _MDB_dbi))
        -> (s : _int)
        -> (begin
             (check-status s)
             d)))

(deflmdb mdb_dbi_flags
  (_fun _MDB_txn-pointer
        _MDB_dbi
        (f : (_ptr o _mdb_db_flags))
        -> (s : _int)
        -> (begin
             (check-status s)
             f)))

;; Returns value if found or #f on missing entry rather than raising
;; MDB_NOTFOUND.
(deflmdb mdb_get
  (_fun _MDB_txn-pointer
        _MDB_dbi
        _MDB_val-pointer
        (v : (_ptr o _MDB_val))
        -> (s : _int)
        -> (cond
             [(= s MDB_NOTFOUND)
              #f]
             [else
              (check-status s)
              v]))
  #:wrap (lambda (raw)
           (lambda (txn dbi key)
             (define data (raw txn dbi (bytes->MDB_val key)))
             (and data (MDB_val->bytes data)))))

(deflmdb mdb_put
  (_fun _MDB_txn-pointer
        _MDB_dbi
        _MDB_val-pointer
        _MDB_val-pointer
        _mdb_write_flags
        -> (s : _int)
        -> (check-status s))
  #:wrap (lambda (raw)
           (lambda (txn dbi key data flags)
             (raw txn
                  dbi
                  (bytes->MDB_val key)
                  (bytes->MDB_val data)
                  flags))))

(deflmdb mdb_del
  (_fun _MDB_txn-pointer
        _MDB_dbi
        _MDB_val-pointer
        _MDB_val-pointer/null
        -> _int)
  #:wrap (lambda (raw)
           (lambda (txn dbi key [val #f])
             (check-status
              (raw txn
                   dbi
                   (bytes->MDB_val key)
                   (bytes->MDB_val/null val))))))

(deflmdb mdb_cursor_open
  (_fun _MDB_txn-pointer
        _MDB_dbi
        (p : (_ptr o _MDB_cursor-pointer))
        -> (s : _int)
        -> (begin
             (check-status s)
             p)))

(deflmdb mdb_cursor_close
  (_fun _MDB_cursor-pointer -> _void))

(deflmdb mdb_cursor_renew
  (_fun _MDB_txn-pointer
        _MDB_cursor-pointer
        -> (s : _int)
        -> (check-status s)))

(deflmdb mdb_cursor_txn
  (_fun _MDB_cursor-pointer -> _MDB_txn-pointer))

(deflmdb mdb_cursor_dbi
  (_fun _MDB_cursor-pointer -> _MDB_dbi))

;; Returns boolean indicating if item was found or not.
(deflmdb mdb_cursor_get
  (_fun _MDB_cursor-pointer
        _MDB_val-pointer
        _MDB_val-pointer
        _MDB_cursor_op
        -> _int)
  #:wrap (lambda (raw)
           (lambda (cursor key data op)
             (define in-key (unbox key))
             (define key-val
               (if in-key
                   (bytes->MDB_val in-key)
                   (make-MDB_val 0 #f)))
             (define in-key-mv_data (MDB_val-mv_data key-val))

             (define in-data (unbox data))
             (define data-val
               (if in-data
                   (bytes->MDB_val in-data)
                   (make-MDB_val 0 #f)))
             (define in-data-mv_data (MDB_val-mv_data data-val))

             (define status (raw cursor key-val data-val op))
             ;; Only update boxes if in/out args were touched.
             (unless (equal? in-key-mv_data (MDB_val-mv_data key-val))
               (set-box! key (MDB_val->bytes key-val)))
             (unless (equal? in-data-mv_data (MDB_val-mv_data data-val))
               (set-box! data (MDB_val->bytes data-val)))
             (cond
               [(= status MDB_NOTFOUND)
                #f]
               [else
                (check-status status)
                #t]))))

(deflmdb mdb_cursor_put
  (_fun _MDB_cursor-pointer
        _MDB_val-pointer
        _MDB_val-pointer
        _mdb_write_flags
        -> _int)
  #:wrap (lambda (raw)
           (lambda (cursor key data flags)
             (define key-val (bytes->MDB_val/null (unbox key)))
             (define data-val (bytes->MDB_val/null (unbox data)))
             (check-status (raw cursor key-val data-val flags)))))

(deflmdb mdb_cursor_del
  (_fun _MDB_cursor-pointer
        _mdb_write_flags
        -> (s : _int)
        -> (check-status s)))

(deflmdb mdb_cursor_count
  (_fun _MDB_cursor-pointer
        (c : (_ptr o _size))
        -> (s : _int)
        -> (begin
             (check-status s)
             c)))
