#lang racket/base

(require ffi/unsafe
         "types.rkt"
         "errors.rkt")

(provide (all-defined-out))

; TODO: Make sure we follow error string conventions
(define (check-status s)
  (unless (zero? s)
    (raise (mdb_error "mdb_error" (current-continuation-marks) s))))

(define (bytes->MDB_val b)
  ;; I could provide C wrapper procs that remove much of the need for data
  ;; copying performed by this proc taking the buffers and their sizes as
  ;; arguments instead of taking MDB_val pointers.
  (define length (bytes-length b))
  (define buffer (malloc 'atomic-interior length))
  (memcpy buffer b length)
  (make-MDB_val length buffer))

;; Allow #f (used for NULL in FFI) to pass through.
(define (bytes->MDB_val/null b)
  (and b (bytes->MDB_val b)))

(define (MDB_val->bytes v)
  ;; There may be a better way to do this where we don't initialize the memory
  ;; and then fill over it. One way is to make C wrappers that pass back sizes
  ;; and _bytes directly, rather than MDB_vals.
  (define size (MDB_val-mv_size v))
  (define buffer (make-bytes size))
  (memcpy buffer (MDB_val-mv_data v) size)
  buffer)
