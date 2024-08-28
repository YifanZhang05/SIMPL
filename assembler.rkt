#lang racket

(provide primplify)

;; assembler that converts A-PRIMPL to PRIMPL. For more information about A-PRIMPL and PRIMPL, see README
;; usage: call (primplify lst), where lst is a list of A-PRIMPL instructions.
;; here's an example of a list of A-PRIMPL instructions (it computes 1+2):
(define example '((add sum a b)
                  (print-val sum)
                  (data a 1)
                  (data b 2)
                  (data sum 0)
                  (halt)))
;; to assemble the A-PRIMPL code above, call (primplify example). It produces the following PRIMPL code:
;       '((add (4) (2) (3)) (print-val (4)) 1 2 0 0)
;; to run PRIMPL code, see run-PRIMPL.rkt

(define-struct const (n) #:transparent)
(define-struct label (n) #:transparent)
(define-struct data (n) #:transparent)
(define (struct-n s)
  (cond
    [(const? s) (const-n s)]
    [(label? s) (label-n s)]
    [(data? s) (data-n s)]
    [true s]))
(define (same-struct s value) ; return a struct with same type as s and with given value
  (cond
    [(const? s) (const value)]
    [(label? s) (label value)]
    [(data? s) (data value)]
    [true value]))

(define (check-error hashmap)
  (define m (make-hash))
  (define i 0)
  (hash-for-each hashmap
                 (lambda(key value)
                   (if (empty? (hash-ref m key (lambda() empty)))
                       (hash-set! m key 0)
                       (error 'duplicate))))
  (define (circle-check key i)
    ;(display key)(newline)
    (cond
      [(symbol? (struct-n (hash-ref hashmap key (lambda() (error 'undefined)))))
       (define new-key (struct-n (hash-ref hashmap key (lambda() (error 'undefined)))))
       ;(display new-key)
       (cond
         [(= i (hash-ref m new-key (lambda() (error 'undefined)))) (error 'circular_definition)]
         [(= 0 (hash-ref m new-key (lambda() (error 'undefined)))) (hash-set! m key i) (circle-check new-key i)]
         [true (hash-set! m key i)]
         )]
      [true (hash-set! m key i)]))
  (hash-for-each m (lambda(key value) (set! i (add1 i)) (circle-check key i))))

(define hashmap (make-hash))
;; takes a list of A-PRIMPL instructions, modifies hashmap with symbols as keys and their associated values
;; i is memory location of current instruction, start at 1
;; return a new list of instruction in PRIMPL format. This parameter is passed in as empty initially
(define (hash-symbols instructions i new-instr)
  ;(display (car instructions))
  (if (empty? instructions) new-instr  ; this function returns hashmap and updated instruction list
      (match (car instructions)
        [`(halt)
         (hash-symbols (cdr instructions) (add1 i) (cons 0 new-instr))]
        [`(label ,psymbol)
         (if (hash-has-key? hashmap psymbol)
             (error 'duplicate)
             (begin
               (hash-set! hashmap psymbol (label i))
               (hash-symbols (cdr instructions) i new-instr)))]
        ; later deal with error for repeat psymbol
        [`(const ,psymbol ,value)
         (if (hash-has-key? hashmap psymbol)
             (error 'duplicate)
             (begin
               (hash-set! hashmap psymbol (const value))
               (hash-symbols (cdr instructions) i new-instr)))]
        [`(lit ,value)
         (hash-symbols (cdr instructions) (add1 i) (cons value new-instr))]
        [`(data ,name (,n ,value))
         (cond
           [(hash-has-key? hashmap name) (error 'duplicate)]
           [true (define lst (make-list n value))
                 (hash-set! hashmap name (data i))
                 (hash-symbols (cdr instructions) (+ i n) (append lst new-instr))])]
        [(cons 'data body)
         (cond
           [(hash-has-key? hashmap (car body)) (error 'duplicate)]
           [true (define psymbol (car body))
                 (define lst (cdr body))
                 (define instr-with-lst (append (reverse lst) new-instr))
                 (hash-set! hashmap psymbol (data i))
                 (hash-symbols (cdr instructions) (+ i (length lst)) instr-with-lst)])]
        [x (hash-symbols (cdr instructions) (add1 i) (cons x new-instr))])))

;; resolve mapping so symbols map to numbers
(define (resolve-map)
  (hash-for-each hashmap (lambda(key value) (resolve-key key value))))
(define (resolve-key key value)
  (cond
    [(symbol? (struct-n value))
     ;(display key) (newline)
     (define actual-value (hash-ref hashmap (struct-n value)))
     ;(display actual-value) (newline)
     (cond
       [(symbol? (struct-n actual-value))
        ;(display actual-value) (newline)
        (define final-value (resolve-key (struct-n value) actual-value))
        (hash-set! hashmap key (same-struct value (struct-n final-value)))
        final-value]
       [true (hash-set! hashmap key (same-struct value (struct-n actual-value))) actual-value])]
    [true value]))

(define (primplify instructions)
  (set! hashmap (make-hash))
  (define new-instruction (reverse (hash-symbols instructions 0 empty)))
  (check-error hashmap)
  (resolve-map)
  (define (primplify-helper lst newlst)
    (cond
      [(empty? lst) newlst]
      [true
       ;(display (car lst)) (newline)
       (match (car lst)
         [`(jump ,opd)
          (primplify-helper (cdr lst) (cons (list 'jump (resolve-opd-loc opd)) newlst))]
         [`(jsr ,opd1 ,opd2)
          (primplify-helper (cdr lst) (cons (list 'jsr (resolve-dest opd1) (resolve-opd-loc opd2)) newlst))]
         [`(branch ,opd1 ,opd2)
          (define new1 (resolve-opd opd1))
          (define new2 (resolve-opd-loc opd2))
          (primplify-helper (cdr lst) (cons (list 'branch new1 new2) newlst))]
         [`(print-val ,opd)
          (primplify-helper (cdr lst) (cons (list 'print-val (resolve-opd opd)) newlst))]
         [`(print-string ,str)
          (primplify-helper (cdr lst) (cons (list 'print-string str) newlst))]
         [(cons instr para)
          (define dest (car para))
          (define opds (cdr para))
          ;(display dest) (newline)
          (primplify-helper (cdr lst)
                            (cons (cons instr (cons (resolve-dest dest)
                                                    (map (lambda(x) (resolve-opd x)) opds))) newlst))]
         [x
          (if (or (boolean? x) (number? x))
              (primplify-helper (cdr lst) (cons x newlst))
              (primplify-helper (cdr lst) (cons (struct-n (hash-ref hashmap x)) newlst)))])]))
  (reverse (primplify-helper new-instruction empty)))

(define (resolve-opd-loc opd)
  (cond
    [(symbol? opd) (if (label? (hash-ref hashmap opd (lambda()(error 'undefined))))
                       (struct-n (hash-ref hashmap opd))
                       (list (struct-n (hash-ref hashmap opd))))]
    [true (resolve-opd opd)]))

(define (resolve-opd imm)
  (cond
    [(or (boolean? imm) (number? imm)) imm]
    [(symbol? imm)
     (if (hash-has-key? hashmap imm)
         (cond
           [(const? (hash-ref hashmap imm)) (struct-n (hash-ref hashmap imm))]
           [(data? (hash-ref hashmap imm)) (list (struct-n (hash-ref hashmap imm)))]
           [(label? (hash-ref hashmap imm)) (error 'incorrect_label_use)])
         (error 'undefined))]
    [(list? imm)
     (if (= 1 (length imm))
         imm
         (cons (get-direct (car imm))
               (map (lambda(x) (resolve-dest x)) (cdr imm))))]))
(define (get-direct n)
  (cond
    [(symbol? n)
     (if (hash-has-key? hashmap n)
         (cond
           [(const? (hash-ref hashmap n)) (struct-n (hash-ref hashmap n))]
           [(data? (hash-ref hashmap n)) (struct-n (hash-ref hashmap n))]
           [(label? (hash-ref hashmap n)) (error 'incorrect_label_use)])
         (error 'undefined))]
    [true n]))
(define (resolve-dest ind)
  ;(display ind) (newline)
  (cond
    [(number? ind) ind]
    [(symbol? ind)
     (if (hash-has-key? hashmap ind)
         (cond
           [(const? (hash-ref hashmap ind)) (error 'incorrect_const_use)]
           [(data? (hash-ref hashmap ind)) (list (struct-n (hash-ref hashmap ind)))]
           [(label? (hash-ref hashmap ind)) (error 'incorrect_label_use)])
         (error 'undefined))]
    [(list? ind)
     (if (= 1 (length ind))
         ind
         (cons (get-direct (car ind))
               (map (lambda(x) (resolve-dest x)) (cdr ind))))]))

; treat every element in lst as key, replace them with value in hashmap
; will replace with immediate or indirect based on the instr and struct of value (data, const, label)
(define (replace lst acc instr) 
  (cond
    [(empty? lst) acc]
    [true
     (if (hash-has-key? hashmap (car lst))
         (replace (cdr lst) (cons (hash-ref hashmap (car lst)) acc) instr)
         (replace (cdr lst) (cons (car lst) acc) instr))]))
