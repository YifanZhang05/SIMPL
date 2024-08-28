#lang racket

(provide compile-simpl)

;; Compiler that converts SIMPL to A-PRIMPL
;; For more information about SIMPL and A-PRIMPL, see README
;; This compiler doesn't support functions.
;; To use functions, see compiler-function.rkt that compiles SIMPLE-F (SIMPLE-Function)
;; How to use:
;    Call (compile-simpl prog), where prog is an S-expression representing the SIMPL program
;    Example: below, test is an S-expression representing SIMPL program that computes and print 2^10.
;             (compile-simpl test) compiles this program
;    After compiling, use assembler.rkt to assemble the program into PRIMPL, then use run-PRIMPL.rkt to run it. 
(define test
  '(vars [(x 10) (y 1)]
  (while (> x 0)
     (set y (* 2 y))
     (set x (- x 1)))
  (print y))
)

;; add underscore prefix to variable name (symbol)
(define (underscore var)
  (cond
    [(equal? var 'true) #t]
    [(equal? var 'false) #f]
    [(symbol? var) (string->symbol (string-append "_" (symbol->string var)))]
    [true var])
  )

;; Convert the list after vars like [(x 3) (y 4)] to list of data statements
(define (convert-vars vars acc)
  (cond
    [(empty? vars) acc]
    [true
     (define var (first (car vars)))
     (define val (second (car vars)))
     (convert-vars (cdr vars)
                   (cons (list 'data (underscore var) val) acc))]))

(define (compile-statement stmt output)
  (match stmt
    [(cons 'vars (cons variables statements))
     (define vars (convert-vars variables (list (list 'data 'SP 'L) (list 'label 'L))))
     (compile-statement (cons 'seq statements) vars)]
    [`(print ,content)
     (cond
       [(string? content) (cons (list 'print-string content) output)]
       [true
        (define compile-content (compile-statement content empty))
        (define pop-stack '((print-val (-1 SP))
                            (sub SP SP 1)))
        (append compile-content (append pop-stack output))])]
    [`(set ,id ,aexp)
     (define compile-aexp (compile-statement aexp empty))
     (define set-id (list (list 'move (underscore id) '(-1 SP))
                          '(sub SP SP 1)))
     (append compile-aexp (append set-id output))]
    [(cons 'seq stmt)
     (compile-seq-helper (reverse stmt) output)]
    [`(iif ,bexp ,stmt1 ,stmt2)
     (define label0 (new-label))
     (define label1 (new-label))
     (define label2 (new-label))
     (define compile-bexp (compile-statement bexp empty))
     (define control-flow1 (list (list 'branch '(-1 SP) label0)
                                 '(sub SP SP 1)
                                 (list 'jump label1)
                                 (list 'label label0)
                                 '(sub SP SP 1)))
     (define compile-stmt1 (compile-statement stmt1 empty))
     (define control-flow2 (list (list 'jump label2)
                                 (list 'label label1)))
     (define compile-stmt2 (compile-statement stmt2 empty))
     (define control-flow3 (list (list 'label label2)))
     (append compile-bexp
             (append control-flow1
                     (append compile-stmt1
                             (append control-flow2
                                     (append compile-stmt2
                                             (append control-flow3 output))))))]
    [`(skip)
     output]
    [(cons 'while (cons bexp stmt))
     (define label0 (new-label))
     (define label1 (new-label))
     (define label2 (new-label))
     (define control-flow1 (list (list 'label label0)))
     (define compile-bexp (compile-statement bexp empty))
     (define control-flow2 (list (list 'branch '(-1 SP) label1)
                                 '(sub SP SP 1)
                                 (list 'jump label2)
                                 (list 'label label1)
                                 '(sub SP SP 1)))
     (define compile-stmt (compile-statement (cons 'seq stmt) empty))
     (define control-flow3 (list (list 'jump label0)
                                 (list 'label label2)))
     (append control-flow1
             (append compile-bexp
                     (append control-flow2
                             (append compile-stmt
                                     (append control-flow3 output)))))]
    [(cons 'and bexp-lst)
     (define bexp1 (car bexp-lst))
     (define bexp2 (cdr bexp-lst))
     (cond
       [(empty? bexp2) (append (compile-statement bexp1 empty) output)]
       [true
        (define compile-bexp1 (compile-statement bexp1 empty))
        (define compile-bexp2 (compile-statement (cons 'and bexp2) empty))
        (define apply-op (list '(land (-2 SP) (-2 SP) (-1 SP))
                               '(sub SP SP 1)))
        (append compile-bexp1 (append compile-bexp2 (append apply-op output)))])]
    [(cons 'or bexp-lst)
     (define bexp1 (car bexp-lst))
     (define bexp2 (cdr bexp-lst))
     (cond
       [(empty? bexp2) (append (compile-statement bexp1 empty) output)]
       [true
        (define compile-bexp1 (compile-statement bexp1 empty))
        (define compile-bexp2 (compile-statement (cons 'or bexp2) empty))
        (define apply-op (list '(lor (-2 SP) (-2 SP) (-1 SP))
                               '(sub SP SP 1)))
        (append compile-bexp1 (append compile-bexp2 (append apply-op output)))])]
    [`(not ,bexp)
     (define compile-bexp (compile-statement bexp empty))
     (define apply-not (list '(lnot (-1 SP) (-1 SP))))
     (append compile-bexp (append apply-not output))]
    [(list op exp1 exp2)
     (define compile-exp1 (compile-statement exp1 empty))
     (define compile-exp2 (compile-statement exp2 empty))
     (define apply-op (list (list (hash-ref dispatch-table op) '(-2 SP) '(-2 SP) '(-1 SP))
                            '(sub SP SP 1)))
     (append compile-exp1 (append compile-exp2 (append apply-op output)))
     ]
    [x
     (define compiled (list (list 'move '(0 SP) (underscore x))
                            (list 'add 'SP 1 'SP)))
     (append compiled output)]))

(define (compile-seq-helper stmt output)
  (cond
       [(= 1 (length stmt)) (compile-statement (car stmt) output)]
       [true (define compile-first (compile-statement (car stmt) output))
             (compile-seq-helper (cdr stmt) compile-first)]))

(define i 0)
(define (new-label)
  (set! i (add1 i))
  (string->symbol (string-append "LABEL" (number->string i))))

(define dispatch-table
  (hash
    '+ 'add
    '- 'sub
    '* 'mul
    '> 'gt
    '>= 'ge
    '< 'lt
    '<= 'le
    '= 'equal
    'and 'land
    'or 'lor
    'not 'lnot
    'div 'div
    'mod 'mod))

(define (compile-simpl prog)
  (compile-statement prog empty))

;(compile-simpl test)
