#lang plai-typed

#|

Contains definitions for some built-in class methods.
We hope to change these to class definitions

|#

(require "python-core-syntax.rkt")

(define (core-id [x : symbol]) : CExp
  (CGet (CIdLHS x)))

;; method for dict.get(key)
(define (dict-get-lambda (self : CVal) (env : Env)) : CVal
  (VMethod self #f (list 'self 'key 'default) (list (CUndefined) (CNone))
    (CLet 'temp-val (CPrim2 'builtin-dict-get (core-id 'self) (core-id 'key))
      ; return default if None
      (CIf (CPrim2 'Eq (core-id 'temp-val) (CNone))
           (CReturn (core-id 'default))
           (CReturn (core-id 'temp-val))))
    env))

;; method for dict.clear()
(define (dict-clear-lambda [self : CVal] [env : Env]) : CVal
  (VMethod self #f (list 'self) empty
    (CPrim1 'builtin-dict-clear (core-id 'self))
    env))

(define (dict-update-lambda [self : CVal] [env : Env]) : CVal
  (VMethod self #f (list 'self 'new-vals) (list (CDict #t (make-hash empty)))
    (CPrim2 'builtin-dict-update (core-id 'self) (core-id 'new-vals))
    env))

(define (dict-keys-lambda [self : CVal] [env : Env]) : CVal
  (VMethod self #f (list 'self) empty
    (CReturn (CPrim1 'builtin-dict-keys (core-id 'self)))
    env))

(define (dict-values-lambda [self : CVal] [env : Env]) : CVal
  (VMethod self #f (list 'self) empty
    (CReturn (CPrim1 'builtin-dict-values (core-id 'self)))
    env))

(define (dict-items-lambda [self : CVal] [env : Env]) : CVal
  (VMethod self #f (list 'self) empty
    (CReturn (CPrim1 'builtin-dict-items (core-id 'self)))
    env))

#|
(define (dict-getitem-lambda [self : CVal] [env : Env]) : CVal
  (VMethod self #f (list 'self) empty
    (CPrim1 '
|#

; for string comparison
(define alphabet
  (make-hash empty))

(begin
  (hash-set! alphabet "a" 97)
  (hash-set! alphabet "b" 98)
  (hash-set! alphabet "c" 99)
  (hash-set! alphabet "d" 100)
  (hash-set! alphabet "e" 101)
  (hash-set! alphabet "f" 102)
  (hash-set! alphabet "g" 103)
  (hash-set! alphabet "h" 104)
  (hash-set! alphabet "i" 105)
  (hash-set! alphabet "j" 106)
  (hash-set! alphabet "k" 107)
  (hash-set! alphabet "l" 108)
  (hash-set! alphabet "m" 109)
  (hash-set! alphabet "n" 110)
  (hash-set! alphabet "o" 111)
  (hash-set! alphabet "p" 112)
  (hash-set! alphabet "q" 113)
  (hash-set! alphabet "r" 114)
  (hash-set! alphabet "s" 115)
  (hash-set! alphabet "t" 116)
  (hash-set! alphabet "u" 117)
  (hash-set! alphabet "v" 118)
  (hash-set! alphabet "w" 119)
  (hash-set! alphabet "x" 120)
  (hash-set! alphabet "y" 121)
  (hash-set! alphabet "z" 122)
  (hash-set! alphabet "A" 65)
  (hash-set! alphabet "B" 66)
  (hash-set! alphabet "C" 67)
  (hash-set! alphabet "D" 68)
  (hash-set! alphabet "E" 69)
  (hash-set! alphabet "F" 70)
  (hash-set! alphabet "G" 71)
  (hash-set! alphabet "H" 72)
  (hash-set! alphabet "I" 73)
  (hash-set! alphabet "J" 74)
  (hash-set! alphabet "K" 75)
  (hash-set! alphabet "L" 76)
  (hash-set! alphabet "M" 77)
  (hash-set! alphabet "N" 78)
  (hash-set! alphabet "O" 79)
  (hash-set! alphabet "P" 80)
  (hash-set! alphabet "Q" 81)
  (hash-set! alphabet "R" 82)
  (hash-set! alphabet "S" 83)
  (hash-set! alphabet "T" 84)
  (hash-set! alphabet "U" 85)
  (hash-set! alphabet "V" 86)
  (hash-set! alphabet "W" 87)
  (hash-set! alphabet "X" 88)
  (hash-set! alphabet "Y" 89)
  (hash-set! alphabet "Z" 90))

;; ascii single-char string to its corresponding number
(define (atoi s) : number
  (type-case (optionof number) (hash-ref alphabet s)
    [none () -1]
    [some (v) v]))

(define (string-to-chars [s : string]) : (listof string)
  (map (lambda (c)
         (list->string (list c)))
       (string->list s)))

(define (string-in [needle : string] [haystack : string]) : boolean
  (string-in-helper (string-to-chars needle)
                    (string-to-chars haystack)))

(define (chars-substr [s : (listof string)] [len : number]) : (listof string)
  (let ([n (box 0)])
    (foldl (lambda (c s)
             (begin
               (set-box! n (add1 (unbox n)))
               (if (<= (unbox n) len)
                   (append s (list c))
                   s)))
           empty
           s)))

(define (string-in-helper [needle : (listof string)] [haystack : (listof string)]) : boolean
  (if (< (length haystack) (length needle))
    ; haystack is too small
    #f
    (if (equal? (chars-substr haystack (length needle))
                needle)
        #t
        ; search in later parts of haystack
        (string-in-helper needle (rest haystack)))))

(define (compare-str [s1 : string] [s2 : string]) : number
  (compare-str-helper (string-to-chars s1)
                      (string-to-chars s2)))

(define (compare-str-helper [s1 : (listof string)] [s2 : (listof string)]) : number
  (cond
    ; check emptiness
    [(and (empty? s1) (empty? s2)) 0]
    [(empty? s1) -1]
    [(empty? s2) 1]
    ; compare first char
    [else (local ([define c1 (atoi (first s1))]
                  [define c2 (atoi (first s2))])
            (cond
              [(= c1 c2) (compare-str-helper (rest s1) (rest s2))]
              [(< c1 c2) -1]
              [(> c1 c2) 1]))]))

;; to-number : CVal -> number
;; converts a value to a number
(define (to-number [val : CVal]) : number
  (type-case CVal val
    [VInt (n) n]
    [VFloat (n) n]
    [VTrue () 1]
    [VFalse () 0]
    [else (error 'interp "non-primitive can't be converted to number")]))

(define (numeric? [val : CVal]) : boolean
  (type-case CVal val
    [VInt (n) #t]
    [VFloat (n) #t]
    [VTrue () #t]
    [VFalse () #t]
    [else #f]))


