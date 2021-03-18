#lang racket

(define counter
  (for*/fold ([counter (hash)])
             ([line (in-lines)]
              [word (in-list (string-split line))])
    (hash-update counter (string-downcase word) add1 0)))

(define sorted (sort (hash->list counter) > #:key cdr))

(for ([pair (in-list sorted)])
  (printf "~a ~a~%" (car pair) (cdr pair)))

