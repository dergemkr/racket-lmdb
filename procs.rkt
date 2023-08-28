#lang racket/base

;; The functions in this file are defined in the same order as they're listed in
;; the LMDB API documentation.

(require ffi/unsafe
         racket/string
         "errors.rkt"
         "types.rkt"
         "utils.rkt")

(module+ test
  (require (for-syntax racket/base
                       syntax/parse)
           ffi/unsafe/port
           racket/file
           racket/format
           racket/function
           racket/os
           rackunit)

  (define-syntax (with-env stx)
    (syntax-parse stx
      [(_ (name:id path:expr (~alt (~optional (~seq #:flags flags:expr))
                                   (~optional (~seq #:mode mode:expr))) ...)
          (~optional (~seq #:before-open opener:expr))
          body ...+)
       #'(let ([name (mdb_env_create)]
               [m (~? mode #o664)]
               [f (~? flags '())])
           (~? opener)

           (mdb_env_open name path f m)

           (define return
             (let ()
               body ...))

           ;; This will not happen if there's an exception in body, but it's not a
           ;; problem since nobody else will use the env and we're unlikely to run out
           ;; of resources in testing.
           (mdb_env_close name)

           return)]))

  (define-syntax-rule (with-tmp-env (id . options) body ...)
    (with-env (id (make-temporary-directory "db~a") . options)
      body ...))

  (define-syntax-rule (with-txn (id env parent flags) body ...)
    (let ([committing #f]
          [id (mdb_txn_begin env parent flags)])
      (call-with-exception-handler
       (lambda (e)
         (unless committing
           (mdb_txn_abort id))
         e)
       (lambda ()
         (let () body ...)
         (set! committing #t)
         (mdb_txn_commit id)))))

  (test-case "with-txn"
    (with-tmp-env (e)
      (define d #f)

      ;; Write value to DB and ensure that's committed in happy case.
      (with-txn (x e #f '())
        (set! d (mdb_dbi_open x #f '()))
        (mdb_put x d #"key" #"value1" '()))
      (with-txn (x e #f '())
        (check-equal? (mdb_get x d #"key") #"value1"))

      ;; Write value to DB, throw error, and ensure that value wasn't committed.
      (check-exn
       (lambda (e)
         (and (exn:fail? e)
              (equal? (exn-message e) "marker")))
       (thunk
        (with-txn (x e #f '())
          (mdb_put x d #"key" #"value2" '())
          (error "marker"))))
      (with-txn (x e #f '())
        (check-equal? (mdb_get x d #"key") #"value1")))))

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

(define mdb_env_copy
  (let ()
    (deflmdb mdb_env_copy2
      (_fun _MDB_env-pointer
            _path
            _mdb_copy_flags
            -> (s : _int)
            -> (check-status s)))

    (deflmdb mdb_env_copyfd2
      (_fun _MDB_env-pointer
            _int
            _mdb_copy_flags
            -> (s : _int)
            -> (check-status s)))

    ;; Use define form so proc has correct name.
    (define (mdb_env_copy env path/fd [flags '()])
      (if (integer? path/fd)
          (mdb_env_copyfd2 env path/fd flags)
          (mdb_env_copy2 env path/fd flags)))

    mdb_env_copy))

(module+ test
  (test-case "mdb_env_copy"
    ;; We'll put a test key and value in the environment, then copy it, and
    ;; finally ensure the copies have that same test key and value.
    (define test-key #"test")
    (define test-value #"value")

    (define (check-env path #:flags [flags '()])
      (with-env (e path #:flags (cons 'MDB_RDONLY flags))
        (with-txn (x e #f '(MDB_RDONLY))
          (define d (mdb_dbi_open x #f '()))
          (check-equal? (mdb_get x d test-key) test-value))))

    (with-tmp-env (e)
      (with-txn (x e #f '())
        (define d (mdb_dbi_open x #f '()))
        (mdb_put x d test-key test-value '()))


      ;; Copy to path, no flags
      (define path1 (make-temporary-directory "db~a"))
      (mdb_env_copy e path1)
      (check-env path1)

      ;; Copy to path with flags
      (define path2 (make-temporary-directory "db~a"))
      (mdb_env_copy e path2)
      (check-env path2)

      ;; Copy to fd, no flags
      (define path3 (make-temporary-file "db~a"))
      (define fd3 (open-output-file path3 #:mode 'binary #:exists 'truncate/replace))
      (mdb_env_copy e (unsafe-port->file-descriptor fd3))
      (check-env path3 #:flags '(MDB_NOSUBDIR))

      ;; Copy to fd with flags
      (define path4 (make-temporary-file "db~a"))
      (define fd4 (open-output-file path4 #:mode 'binary #:exists 'truncate/replace))
      (mdb_env_copy e (unsafe-port->file-descriptor fd4) '(MDB_CP_COMPACT))
      (check-env path4 #:flags '(MDB_NOSUBDIR)))))

(deflmdb mdb_env_stat
  (_fun _MDB_env-pointer
        (t : (_ptr o _MDB_stat))
        -> (s : _int)
        -> (begin
             (check-status s)
             t)))

(deflmdb mdb_env_info
  (_fun _MDB_env-pointer
        (i : (_ptr o _MDB_envinfo))
        -> (s : _int)
        -> (begin
             (check-status s)
             i)))

(module+ test
  (test-case "mdb_env_info/env_set_mapsize"
    (with-tmp-env (e)
      ;; Check the map size of the envinfo struct.
      (define default-mapsize 1048576)
      (check-equal? (MDB_envinfo-me_mapsize (mdb_env_info e)) default-mapsize)
      (define new-mapsize (* default-mapsize 2))
      (mdb_env_set_mapsize e new-mapsize)
      (check-equal? (MDB_envinfo-me_mapsize (mdb_env_info e)) new-mapsize))))

(deflmdb mdb_env_sync
  (_fun _MDB_env-pointer
        _bool
        -> (s : _int)
        -> (check-status s)))

(module+ test
  (test-case "mdb_env_sync"
    ;; We'll create an environment and then we'll open it in read-only mode and
    ;; do a sync on it, which should fail since it's not allowed on read-only
    ;; environments.
    (define path (make-temporary-directory "db~a"))
    (with-env (e path)
      ;; Just creating an empty environment.
      (void))

    (with-env (e path #:flags '(MDB_RDONLY))
      (define errno-eacces (lookup-errno 'EACCES))
      (define (exn:mdb-eacces? e)
        (and (exn:mdb? e)
             (equal? (exn:mdb-code e) errno-eacces)))
      (check-exn exn:mdb-eacces?
                 (thunk (env-sync e #f))))))

(deflmdb mdb_env_close
  (_fun _MDB_env-pointer -> _void))

(deflmdb mdb_env_set_flags
  (_fun _MDB_env-pointer
        _mdb_env_flags
        _bool
        -> (s : _int)
        -> (check-status s)))

(deflmdb mdb_env_get_flags
  (_fun _MDB_env-pointer
        (f : (_ptr o _mdb_env_flags))
        -> (s : _int)
        -> (begin
             (check-status s)
             f)))

(module+ test
  (test-case "mdb_env_get_flags/set_flags"
    (with-tmp-env (e)
      (check-false (member 'MDB_NOSYNC (mdb_env_get_flags e)))
      (mdb_env_set_flags e '(MDB_NOSYNC) #t)
      (check-not-false (member 'MDB_NOSYNC (mdb_env_get_flags e)))
      (mdb_env_set_flags e '(MDB_NOSYNC) #f)
      (check-false (member 'MDB_NOSYNC (mdb_env_get_flags e))))))

(deflmdb mdb_env_get_path
  (_fun _MDB_env-pointer
        (p : (_ptr o _path))
        -> (s : _int)
        -> (begin
             (check-status s)
             p)))

(module+ test
  (test-case "mdb_env_get_path"
    (define path (make-temporary-directory "db~a"))
    (with-env (e path)
      (check-equal? (mdb_env_get_path e) path))))

(deflmdb mdb_env_get_fd
  (_fun _MDB_env-pointer
        (f : (_ptr o _int))
        -> (s : _int)
        -> (begin
             (check-status s)
             f)))

(deflmdb mdb_env_set_mapsize
  (_fun _MDB_env-pointer
        _size
        -> (s : _int)
        -> (check-status s)))

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

(deflmdb mdb_env_get_maxkeysize
  (_fun _MDB_env-pointer
        -> _int))

(module+ test
  (test-case "mdb_env_get_maxkeysize"
    (with-tmp-env (e)
      ;; We can hard-code this constant since we bundle the LMDB library. Not a
      ;; great test, but at least it makes sure the function is hooked up.
      (check-equal? (mdb_env_get_maxkeysize e) 511))))

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

(deflmdb mdb_stat
  (_fun _MDB_txn-pointer
        _MDB_dbi
        (t : (_ptr o _MDB_stat))
        -> (s : _int)
        -> (begin
             (check-status s)
             t)))

(deflmdb mdb_dbi_flags
  (_fun _MDB_txn-pointer
        _MDB_dbi
        (f : (_ptr o _mdb_db_flags))
        -> (s : _int)
        -> (begin
             (check-status s)
             f)))

(deflmdb mdb_dbi_close
  (_fun _MDB_env-pointer
        _MDB_dbi
        -> _void))

(module+ test
  (test-case "mdb_dbi_close"
    (with-tmp-env (e)
      #:before-open (mdb_env_set_maxdbs e 1)

      (define d1 #f)
      (with-txn (x e #f '())
        (set! d1 (mdb_dbi_open x "d1" '(MDB_CREATE)))

        ;; Second open call should fail since we set maxdbs to 1 and d1 is open.
        (define (exn:mdb-dbs-full? e)
          (and (exn:mdb? e)
               (equal? (exn:mdb-code e) MDB_DBS_FULL)))
        (check-exn exn:mdb-dbs-full?
                   (thunk (mdb_dbi_open x "d2" '(MDB_CREATE)))))

      ;; Close d1 after transaction has finished.
      (mdb_dbi_close e d1)

      ;; Open call should succeed since we closed d1.
      (with-txn (x e #f '())
        (mdb_dbi_open x "d2" '(MDB_CREATE))))))

;; Accepts boolean for del instead of 1/0 int.
(deflmdb mdb_drop
  (_fun _MDB_txn-pointer
        _MDB_dbi
        _bool
        -> (s : _int)
        -> (check-status s)))

(module+ test
  (test-case "mdb_drop/env_stat/stat"
    ;; Since I don't want to bind too deeply to the LMDB internals, I'll test
    ;; the number of entries returned by mdb_env_stat and mdb_stat.
    (with-tmp-env (e)
      #:before-open (mdb_env_set_maxdbs e 1)

      ;; No named DBs in the environment yet
      (check-equal? (MDB_stat-ms_entries (mdb_env_stat e)) 0)

      (define d #f)
      (with-txn (x e #f '())
        (set! d (mdb_dbi_open x "d" '(MDB_CREATE)))
        (mdb_put x d #"key1" #"val1" '()))

      (with-txn (x e #f '())
        (check-equal? (MDB_stat-ms_entries (mdb_env_stat e)) 1)
        (check-equal? (MDB_stat-ms_entries (mdb_stat x d)) 1)
        (mdb_drop x d #f))

      (with-txn (x e #f '())
        (check-equal? (MDB_stat-ms_entries (mdb_env_stat e)) 1)
        (check-equal? (MDB_stat-ms_entries (mdb_stat x d)) 0)
        (mdb_drop x d #t))

      (check-equal? (MDB_stat-ms_entries (mdb_env_stat e)) 0))))

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

(module+ test
    (with-tmp-env (e)
  (test-case "mdb_del/get/put"
      (define (exn:mdb-notfound? e)
        (and (exn:mdb? e)
             (equal? (exn:mdb-code e) MDB_NOTFOUND)))
      (with-txn (x e #f '())
        (define d (mdb_dbi_open x #f '()))
        (check-false (mdb_get x d #"test-key"))
        (check-exn exn:mdb-notfound?
                   (thunk (check-false (mdb_del x d #"test-key" #f))))
        (mdb_put x d #"test-key" #"test-value" '())
        (check-equal? (mdb_get x d #"test-key") #"test-value")
        (mdb_del x d #"test-key")
        (check-false (mdb_get x d #"test-key"))
        (check-exn exn:mdb-notfound?
                   (thunk (check-false (mdb_del x d #"test-key" #f))))))))

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

(deflmdb mdb_reader_list
  (_fun _MDB_env-pointer
        _MDB_msg_func
        _pointer
        -> (s : _int)
        -> (check-status s))
  #:wrap (lambda (proc)
           (lambda (env)
             (define log (open-output-string))
             (define (log-append msg ctx)
               (display msg log)
               0)
             (proc env log-append #f)
             (get-output-string log))))

(module+ test
  (test-case "mdb_reader_check"
    (define path (make-temporary-directory "db~a"))
    (with-env (e path)
      (with-txn (x e #f  '(MDB_RDONLY))
        (define l (mdb_reader_list e))
        (check-true (string-contains? l "pid"))
        (check-true (string-contains? l "thread"))
        (check-true (string-contains? l "txnid"))
        (check-true (string-contains? l (~a (getpid))))
        (check-true (string-contains? l (~a (mdb_txn_id x))))))))

(deflmdb mdb_reader_check
  (_fun _MDB_env-pointer
        (d : (_ptr o _int))
        -> (s : _int)
        -> (begin
             (check-status s)
             d)))

(module+ test
  (test-case "mdb_reader_check"
    ;; Don't know how to force a stale reader. Dumb test, but it ensures the
    ;; proc is hooked up.
    (with-tmp-env (e)
      (check-equal? (mdb_reader_check e) 0))))
