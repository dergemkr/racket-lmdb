#lang racket/base

(provide (all-defined-out))

;; Error codes up to date as of 0.9.29/2021-03-16
(define MDB_SUCCESS 0)
(define MDB_KEYEXIST -30799)
(define MDB_NOTFOUND -30798)
(define MDB_PAGE_NOTFOUND -30797)
(define MDB_CORRUPTED -30796)
(define MDB_PANIC -30795)
(define MDB_VERSION_MISMATCH -30794)
(define MDB_INVALID -30793)
(define MDB_MAP_FULL -30792)
(define MDB_DBS_FULL -30791)
(define MDB_READERS_FULL -30790)
(define MDB_TLS_FULL -30789)
(define MDB_TXN_FULL -30788)
(define MDB_CURSOR_FULL -30787)
(define MDB_PAGE_FULL -30786)
(define MDB_MAP_RESIZED -30785)
(define MDB_INCOMPATIBLE -30784)
(define MDB_BAD_RSLOT -30783)
(define MDB_BAD_TXN -30782)
(define MDB_BAD_VALSIZE -30781)
(define MDB_BAD_DBI -30780)

(struct exn:mdb exn:fail (code))
