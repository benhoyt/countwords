#lang racket

(define counter (make-hash))

(let loop ()
  (define line (read-line))
  (unless (eof-object? line)
    (define words (string-split (string-downcase line)))
    (for ([word words])
      (hash-update! counter word add1 0))
    (loop)))

(define counter-list (hash->list counter))
(define sorted (sort counter-list > #:key cdr))

(for ([pair sorted])
  (printf "~a ~a~%" (car pair) (cdr pair)))
