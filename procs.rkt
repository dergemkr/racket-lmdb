#lang racket/base

;; The functions in this file are defined in the same order as they're listed in
;; the LMDB API documentation.

(require ffi/unsafe
         racket/format
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
       #'(let ([name (env-create)]
               [m (~? mode #o664)]
               [f (~? flags '())])
           (~? opener)

           (env-open name path f m)

           (define return
             (let ()
               body ...))

           ;; This will not happen if there's an exception in body, but it's not
           ;; a problem since nobody else will use the env and we're unlikely to
           ;; run out of resources in testing.
           (env-close name)

           return)]))

  (define-syntax-rule (with-tmp-env (id . options) body ...)
    (with-env (id (make-temporary-directory "db~a") . options)
      body ...)))

(provide (except-out (all-defined-out)
                     check-status))

(define (check-status s)
  (unless (zero? s)
    (raise (exn:mdb (format "exn:mdb: ~a" (or (mdb-strerror s)
                                              (~a "Error code " s)))
                    (current-continuation-marks)
                    s))))

(deflmdb mdb-version
  (_fun
   (major : (_ptr o _int))
   (minor : (_ptr o _int))
   (patch : (_ptr o _int))
   -> (readable : _string)
   -> (values major minor patch readable))
  #:c-id mdb_version)

(module+ test
  (test-case "mdb-version"
    (define-values (major minor patch readable) (mdb-version))
    ;; We can hard-code this since we bundle the library right now.
    (check-equal? major 0)
    (check-equal? minor 9)
    (check-equal? patch 29)
    (check-true (string-contains? readable "0.9.29"))))

(deflmdb mdb-strerror
  (_fun _int -> _string)
  #:c-id mdb_strerror)

(deflmdb env-create
  (_fun (o : (_ptr o _MDB_env-pointer))
        -> (s : _int)
        -> (begin
             (check-status s)
             o))
  #:c-id mdb_env_create)

(deflmdb env-open
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
                  mode)))
  #:c-id mdb_env_open)

(define env-copy
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
  (test-case "env-copy"
    ;; We'll put a test key and value in the environment, then copy it, and
    ;; finally ensure the copies have that same test key and value.
    (define test-key #"test")
    (define test-value #"value")

    (define (check-env path #:flags [flags '()])
      (with-env (e path #:flags (cons 'MDB_RDONLY flags))
        (with-txn (x e #f '(MDB_RDONLY))
          (define d (dbi-open x #f '()))
          (check-equal? (get x d test-key) test-value))))

    (with-tmp-env (e)
      (with-txn (x e #f '())
        (define d (dbi-open x #f '()))
        (put x d test-key test-value '()))


      ;; Copy to path, no flags
      (define path1 (make-temporary-directory "db~a"))
      (env-copy e path1)
      (check-env path1)

      ;; Copy to path with flags
      (define path2 (make-temporary-directory "db~a"))
      (env-copy e path2)
      (check-env path2)

      ;; Copy to fd, no flags
      (define path3 (make-temporary-file "db~a"))
      (define fd3 (open-output-file path3 #:mode 'binary #:exists 'truncate/replace))
      (env-copy e (unsafe-port->file-descriptor fd3))
      (check-env path3 #:flags '(MDB_NOSUBDIR))

      ;; Copy to fd with flags
      (define path4 (make-temporary-file "db~a"))
      (define fd4 (open-output-file path4 #:mode 'binary #:exists 'truncate/replace))
      (env-copy e (unsafe-port->file-descriptor fd4) '(MDB_CP_COMPACT))
      (check-env path4 #:flags '(MDB_NOSUBDIR)))))

(deflmdb env-stat
  (_fun _MDB_env-pointer
        (t : (_ptr o _MDB_stat))
        -> (s : _int)
        -> (begin
             (check-status s)
             t))
  #:c-id mdb_env_stat)

(deflmdb env-info
  (_fun _MDB_env-pointer
        (i : (_ptr o _MDB_envinfo))
        -> (s : _int)
        -> (begin
             (check-status s)
             i))
  #:c-id mdb_env_info)

(module+ test
  (test-case "env-info/env-set-mapsize"
    (with-tmp-env (e)
      ;; Check the map size of the envinfo struct.
      (define default-mapsize 1048576)
      (check-equal? (MDB_envinfo-me_mapsize (env-info e)) default-mapsize)
      (define new-mapsize (* default-mapsize 2))
      (env-set-mapsize e new-mapsize)
      (check-equal? (MDB_envinfo-me_mapsize (env-info e)) new-mapsize))))

(deflmdb env-sync
  (_fun _MDB_env-pointer
        _bool
        -> (s : _int)
        -> (check-status s))
  #:c-id mdb_env_sync)

(module+ test
  (test-case "env-sync"
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

(deflmdb env-close
  (_fun _MDB_env-pointer -> _void)
  #:c-id mdb_env_close)

(deflmdb env-set-flags
  (_fun _MDB_env-pointer
        _mdb_env_flags
        _bool
        -> (s : _int)
        -> (check-status s))
  #:c-id mdb_env_set_flags)

(deflmdb env-get-flags
  (_fun _MDB_env-pointer
        (f : (_ptr o _mdb_env_flags))
        -> (s : _int)
        -> (begin
             (check-status s)
             f))
  #:c-id mdb_env_get_flags)

(module+ test
  (test-case "env-get-flags/env-set-flags"
    (with-tmp-env (e)
      (check-false (member 'MDB_NOSYNC (env-get-flags e)))
      (env-set-flags e '(MDB_NOSYNC) #t)
      (check-not-false (member 'MDB_NOSYNC (env-get-flags e)))
      (env-set-flags e '(MDB_NOSYNC) #f)
      (check-false (member 'MDB_NOSYNC (env-get-flags e))))))

(deflmdb env-get-path
  (_fun _MDB_env-pointer
        (p : (_ptr o _path))
        -> (s : _int)
        -> (begin
             (check-status s)
             p))
  #:c-id mdb_env_get_path)

(module+ test
  (test-case "env-get-path"
    (define path (make-temporary-directory "db~a"))
    (with-env (e path)
      (check-equal? (env-get-path e) path))))

(deflmdb env-get-fd
  (_fun _MDB_env-pointer
        (f : (_ptr o _int))
        -> (s : _int)
        -> (begin
             (check-status s)
             f))
  #:c-id mdb_env_get_fd)

(deflmdb env-set-mapsize
  (_fun _MDB_env-pointer
        _size
        -> (s : _int)
        -> (check-status s))
  #:c-id mdb_env_set_mapsize)

(deflmdb env-set-maxreaders
  (_fun _MDB_env-pointer
        _uint
        -> (s : _int)
        -> (check-status s))
  #:c-id mdb_env_set_maxreaders)

(deflmdb env-get-maxreaders
  (_fun _MDB_env-pointer
        (r : (_ptr o _uint))
        -> (s : _int)
        -> (begin
             (check-status s)
             r))
  #:c-id mdb_env_get_maxreaders)

(deflmdb env-set-maxdbs
  (_fun _MDB_env-pointer
        _MDB_dbi
        -> (s : _int)
        -> (check-status s))
  #:c-id mdb_env_set_maxdbs)

(deflmdb env-get-maxkeysize
  (_fun _MDB_env-pointer
        -> _int)
  #:c-id mdb_env_get_maxkeysize)

(module+ test
  (test-case "env-get-maxkeysize"
    (with-tmp-env (e)
      ;; We can hard-code this constant since we bundle the LMDB library. Not a
      ;; great test, but at least it makes sure the function is hooked up.
      (check-equal? (env-get-maxkeysize e) 511))))

(deflmdb txn-begin
  (_fun _MDB_env-pointer
        _MDB_txn-pointer/null
        _mdb_env_flags
        (x : (_ptr o _MDB_txn-pointer))
        -> (s : _int)
        -> (begin
             (check-status s)
             x))
  #:c-id mdb_txn_begin)

(deflmdb txn-env
  (_fun _MDB_txn-pointer
        -> _MDB_env-pointer)
  #:c-id mdb_txn_env)

(deflmdb txn-id
  (_fun _MDB_txn-pointer
        -> _size)
  #:c-id mdb_txn_id)

(deflmdb txn-commit
  (_fun _MDB_txn-pointer
        -> (s : _int)
        -> (check-status s))
  #:c-id mdb_txn_commit)

(deflmdb txn-abort
  (_fun _MDB_txn-pointer -> _void)
  #:c-id mdb_txn_abort)

(deflmdb txn-reset
  (_fun _MDB_txn-pointer -> _void)
  #:c-id mdb_txn_reset)

(deflmdb txn-renew
  (_fun _MDB_txn-pointer
        -> (s : _int)
        -> (check-status s))
  #:c-id mdb_txn_renew)

;; Helper utility to manage txn-begin txn-commit/txn-abort
(define-syntax-rule (with-txn (id env parent flags) body ...)
  (let ([committing #f]
        [id (txn-begin env parent flags)])
    (call-with-exception-handler
     (lambda (e)
       (unless committing
         (txn-abort id))
       e)
     (lambda ()
       (begin0
         (let () body ...)
         (set! committing #t)
         (txn-commit id))))))

(module+ test
  (test-case "with-txn"
    (with-tmp-env (e)
      (define d
        (with-txn (x e #f '())
          (dbi-open x #f '())))

      ;; Write value to DB and ensure that's committed in happy case.
      (with-txn (x e #f '())
        (put x d #"key" #"value1" '()))
      (with-txn (x e #f '())
        (check-equal? (get x d #"key") #"value1"))

      ;; Write value to DB, throw error, and ensure that value wasn't committed.
      (check-exn
       (lambda (e)
         (and (exn:fail? e)
              (equal? (exn-message e) "marker")))
       (thunk
        (with-txn (x e #f '())
          (put x d #"key" #"value2" '())
          (error "marker"))))
      (with-txn (x e #f '())
        (check-equal? (get x d #"key") #"value1")))))

(deflmdb dbi-open
  (_fun _MDB_txn-pointer
        _string
        _mdb_db_flags
        (d : (_ptr o _MDB_dbi))
        -> (s : _int)
        -> (begin
             (check-status s)
             d))
  #:c-id mdb_dbi_open)

(deflmdb stat
  (_fun _MDB_txn-pointer
        _MDB_dbi
        (t : (_ptr o _MDB_stat))
        -> (s : _int)
        -> (begin
             (check-status s)
             t))
  #:c-id mdb_stat)

(deflmdb dbi-flags
  (_fun _MDB_txn-pointer
        _MDB_dbi
        (f : (_ptr o _mdb_db_flags))
        -> (s : _int)
        -> (begin
             (check-status s)
             f))
  #:c-id mdb_dbi_flags)

(deflmdb dbi-close
  (_fun _MDB_env-pointer
        _MDB_dbi
        -> _void)
  #:c-id mdb_dbi_close)

(module+ test
  (test-case "dbi-close"
    (with-tmp-env (e)
      #:before-open (env-set-maxdbs e 1)

      (define d1
        (with-txn (x e #f '())
          (dbi-open x "d1" '(MDB_CREATE))))

      ;; Second open call should fail since we set maxdbs to 1 and d1 is open.
      (with-txn (x e #f '())
        (define (exn:mdb-dbs-full? e)
          (and (exn:mdb? e)
               (equal? (exn:mdb-code e) MDB_DBS_FULL)))
        (check-exn exn:mdb-dbs-full?
                   (thunk (dbi-open x "d2" '(MDB_CREATE)))))

      ;; Close d1 after transaction has finished.
      (dbi-close e d1)

      ;; Open call should succeed since we closed d1.
      (with-txn (x e #f '())
        (define d2 (dbi-open x "d2" '(MDB_CREATE)))
        ;; Just don't want Racket printing value of d2.
        (void)))))

;; Accepts boolean for del instead of 1/0 int.
(deflmdb dbi-drop
  (_fun _MDB_txn-pointer
        _MDB_dbi
        _bool
        -> (s : _int)
        -> (check-status s))
  #:c-id mdb_drop)

(module+ test
  (test-case "dbi-drop/env-stat/stat"
    ;; Since I don't want to bind too deeply to the LMDB internals, I'll test
    ;; the number of entries returned by mdb_env_stat and mdb_stat.
    (with-tmp-env (e)
      #:before-open (env-set-maxdbs e 1)

      ;; No named DBs in the environment yet
      (check-equal? (MDB_stat-ms_entries (env-stat e)) 0)

      (define d
        (with-txn (x e #f '())
          (dbi-open x "d" '(MDB_CREATE))))

      (with-txn (x e #f '())
        (put x d #"key1" #"val1" '()))

      (with-txn (x e #f '())
        (check-equal? (MDB_stat-ms_entries (env-stat e)) 1)
        (check-equal? (MDB_stat-ms_entries (stat x d)) 1)
        (dbi-drop x d #f))

      (with-txn (x e #f '())
        (check-equal? (MDB_stat-ms_entries (env-stat e)) 1)
        (check-equal? (MDB_stat-ms_entries (stat x d)) 0)
        (dbi-drop x d #t))

      (check-equal? (MDB_stat-ms_entries (env-stat e)) 0))))

;; Returns value if found or #f on missing entry rather than raising
;; MDB_NOTFOUND.
(deflmdb get
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
             (and data (MDB_val->bytes data))))
  #:c-id mdb_get)

(deflmdb put
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
                  flags)))
  #:c-id mdb_put)

(deflmdb del
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
                   (bytes->MDB_val/null val)))))
  #:c-id mdb_del)

(module+ test
  (test-case "del/get/put"
    (with-tmp-env (e)
      (define (exn:mdb-notfound? e)
        (and (exn:mdb? e)
             (equal? (exn:mdb-code e) MDB_NOTFOUND)))
      (with-txn (x e #f '())
        (define d (dbi-open x #f '()))
        (check-false (get x d #"test-key"))
        (check-exn exn:mdb-notfound?
                   (thunk (check-false (del x d #"test-key" #f))))
        (put x d #"test-key" #"test-value" '())
        (check-equal? (get x d #"test-key") #"test-value")
        (del x d #"test-key")
        (check-false (get x d #"test-key"))
        (check-exn exn:mdb-notfound?
                   (thunk (check-false (del x d #"test-key" #f))))))))

(deflmdb cursor-open
  (_fun _MDB_txn-pointer
        _MDB_dbi
        (p : (_ptr o _MDB_cursor-pointer))
        -> (s : _int)
        -> (begin
             (check-status s)
             p))
  #:c-id mdb_cursor_open)

(deflmdb cursor-close
  (_fun _MDB_cursor-pointer -> _void)
  #:c-id mdb_cursor_close)

(define-syntax-rule (with-cursor (id txn dbi) body ...)
  (let ([done #f]
        [id (cursor-open txn dbi)])
    (call-with-exception-handler
     (lambda (e)
       (unless done
         (cursor-close id))
       e)
     (lambda ()
       (begin0
           (let () body ...)
         (set! done #t)
         (cursor-close id))))))

(deflmdb cursor-renew
  (_fun _MDB_txn-pointer
        _MDB_cursor-pointer
        -> (s : _int)
        -> (check-status s))
  #:c-id mdb_cursor_renew)

(deflmdb cursor-txn
  (_fun _MDB_cursor-pointer -> _MDB_txn-pointer)
  #:c-id mdb_cursor_txn)

(deflmdb cursor-dbi
  (_fun _MDB_cursor-pointer -> _MDB_dbi)
  #:c-id mdb_cursor_dbi)

(module+ test
  (test-case "cursor-dbi/cursor-renew/cursor-txn"
    (with-tmp-env (e)
      (define d
        (with-txn (x1 e #f '())
          (dbi-open x1 #f '())))

      (define c #f)
      (with-txn (x2 e #f '(MDB_RDONLY))
        (set! c (cursor-open x2 d))
        (check-equal? (cursor-dbi c) d)
        (check-equal? (cursor-txn c) x2))

      ;; Renew swaps cursor in read-only transaction to new read-only
      ;; transaction, keeping cursor on same database.
      (with-txn (x3 e #f '(MDB_RDONLY))
        (cursor-renew x3 c)
        (check-equal? (cursor-dbi c) d)
        (check-equal? (cursor-txn c) x3)))))

;; Returns boolean indicating if item was found or not.
(deflmdb cursor-get
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
                #t])))
  #:c-id mdb_cursor_get)

(module+ test
  (test-case "cursor-get"
    (with-tmp-env (e)
      (with-txn (x e #f '())
        (define d (dbi-open x #f '()))

        (put x d #"test" #"val1" '())

        (with-cursor (c x d)
          (define kb (box #f))
          (define db (box #f))
          (check-true (cursor-get c kb db 'MDB_FIRST))
          (check-equal? (unbox kb) #"test")
          (check-equal? (unbox db) #"val1")
          (check-false (cursor-get c kb db 'MDB_NEXT)))))))

(deflmdb cursor-put
  (_fun _MDB_cursor-pointer
        _MDB_val-pointer
        _MDB_val-pointer
        _mdb_write_flags
        -> _int)
  #:wrap (lambda (raw)
           (lambda (cursor key data flags)
             (define key-val (bytes->MDB_val/null key))
             (define data-val (bytes->MDB_val/null data))
             (check-status (raw cursor key-val data-val flags))))
  #:c-id mdb_cursor_put)

(module+ test
  (test-case "cursor-put"
    (with-tmp-env (e)
      (with-txn (x e #f '())
        (define d (dbi-open x #f '(MDB_DUPSORT)))

        (put x d #"test2" #"val2" '())

        (with-cursor (c x d)
          (cursor-get c (box #"test2") (box #f) 'MDB_SET)
          (cursor-put c #"test3" #"val3" '())
          (cursor-put c #"test1" #"val1" '()))

        (check-equal? (get x d #"test1") #"val1")
        (check-equal? (get x d #"test2") #"val2")
        (check-equal? (get x d #"test3") #"val3")))))

(deflmdb cursor-del
  (_fun _MDB_cursor-pointer
        _mdb_write_flags
        -> (s : _int)
        -> (check-status s))
  #:c-id mdb_cursor_del)

(module+ test
  (test-case "cursor-count"
    (with-tmp-env (e)
      (with-txn (x e #f '())
        (define d (dbi-open x #f '(MDB_DUPSORT)))

        (put x d #"test1" #"val1" '())
        (put x d #"test2" #"val2" '())
        (put x d #"test3" #"val3" '())

        (with-cursor (c x d)
          (cursor-get c (box #"test2") (box #f) 'MDB_SET)
          (cursor-del c '()))

        (check-equal? (get x d #"test1") #"val1")
        (check-equal? (get x d #"test2") #f)
        (check-equal? (get x d #"test3") #"val3")))))

(deflmdb cursor-count
  (_fun _MDB_cursor-pointer
        (c : (_ptr o _size))
        -> (s : _int)
        -> (begin
             (check-status s)
             c))
  #:c-id mdb_cursor_count)

(module+ test
  (test-case "cursor-count"
    (with-tmp-env (e)
      (with-txn (x e #f '())
        (define d (dbi-open x #f '(MDB_DUPSORT)))

        (put x d #"test1" #"val3" '())
        (put x d #"test1" #"val2" '())
        (put x d #"test1" #"val1" '())
        (put x d #"test2" #"val4" '())

        (with-cursor (c x d)
          (cursor-get c (box #f) (box #f) 'MDB_FIRST)
          (check-equal? (cursor-count c) 3))))))

(deflmdb reader-list
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
             (get-output-string log)))
  #:c-id mdb_reader_list)

(module+ test
  (test-case "reader-list"
    (define path (make-temporary-directory "db~a"))
    (with-env (e path)
      (with-txn (x e #f  '(MDB_RDONLY))
        (define l (reader-list e))
        (check-true (string-contains? l "pid"))
        (check-true (string-contains? l "thread"))
        (check-true (string-contains? l "txnid"))
        (check-true (string-contains? l (~a (getpid))))
        (check-true (string-contains? l (~a (txn-id x))))))))

(deflmdb reader-check
  (_fun _MDB_env-pointer
        (d : (_ptr o _int))
        -> (s : _int)
        -> (begin
             (check-status s)
             d))
  #:c-id mdb_reader_check)

(module+ test
  (test-case "reader-check"
    ;; Don't know how to force a stale reader. Dumb test, but it ensures the
    ;; proc is hooked up.
    (with-tmp-env (e)
      (check-equal? (reader-check e) 0))))
