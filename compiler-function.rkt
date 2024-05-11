#lang racket
;; Compiler that converts SIMPL-F (SIMPL-Function) to A-PRIMPL
;; SIMPL-F allows the use of functions. For more info, see README
;; Same use as compiler.rkt. Below is a SIMPL-F program that computes factorial of a x (here x=10, as shown in main)
;; to compile this example program, call (compile-simpl example)
(define example
  '((fun (fact-h n acc)
         (vars []
               (iif (> n 0)
                    (return
                     (fact-h
                      (- n 1)
                      (* n acc)))
                    (return acc))
               (return 0)))
    (fun (main)
         (vars ([x 10])
               (print (fact-h x 1))
               (return 0)))))

(define hash-arg-num (make-hash))

(define (compile-simpl func-lst)
  (set! hash-arg-num (make-hash))
  (check-dup-fname func-lst empty)
  (define compile-other-func (compile-other func-lst empty))
  (define compile-main-func (compile-main func-lst))
  (append compile-main-func
          (append compile-other-func (list '(data RETURN-VAL 0)
                                           '(data RETURN-ADDR 0)
                                           '(data FP 0)
                                           '(data SP END)
                                           '(label END)))))

(define (check-dup-fname func-lst fname-lst)
  (cond
    [(empty? func-lst) (void)]
    [(member (car (second (car func-lst))) fname-lst) (error 'duplicate-function-name)]
    [true
     (hash-set! hash-arg-num (car (second (car func-lst))) (length (cdr (second (car func-lst)))))
     (check-dup-fname (cdr func-lst) (cons (car (second (car func-lst))) fname-lst))]))

(define (check-dup-var-name vars var-names)
  (cond
    [(empty? vars) (void)]
    [(member (car vars) var-names) (error 'duplicate-variable-name)]
    [true (check-dup-var-name (cdr vars) (cons (car vars) var-names))]))

;; compile all functions except for main
(define (compile-other fun-lst acc)
  (cond
    [(empty? fun-lst) acc]
    [(equal? (car (second (car fun-lst))) 'main)
     (compile-other (cdr fun-lst) acc)]
    [true
     (compile-other (cdr fun-lst) (append (compile-function (car fun-lst)) acc))]))

(define (compile-main fun-lst)
  (cond
    [(empty? fun-lst) empty]
    [(equal? (car (second (car fun-lst))) 'main)
     (compile-function (car fun-lst))]
    [true (compile-main (cdr fun-lst))]))

(define (check-return fun-stmts)
  (cond
    [(empty? fun-stmts) (error 'no-return)]
    [(equal? 'return (car (car fun-stmts)))
     (if (empty? (cdr fun-stmts))
         (void)
         (check-return (cdr fun-stmts)))]
    [true (check-return (cdr fun-stmts))]))

(define (compile-function func)
  (define fname (car (second func)))
  (define args (cdr (second func)))
  (define body (third func))
  (define vars (second body))
  (check-dup-var-name (append args (map (lambda(x) (car x)) vars)) empty)
  (define stmts (cons 'seq (cdr (cdr body))))
  (check-return (cdr (cdr body)))
  
  (define prologue (func-prologue fname args vars))
  (define compiled-stmts (compile-statement fname stmts (func-epilogue fname args vars)))
  (append prologue compiled-stmts))

(define (func-prologue fname args vars)
  (define compile-vars (store-vars fname vars (list '(move (0 SP) FP)  ; store FP of the caller in offset 0
                                                    '(move (1 SP) RETURN-ADDR)  ; offset 1: store where called func should return
                                                    '(move FP SP)
                                                    (list 'add 'SP 'SP (+ 2 (length vars))))))
  ;(display compile-vars) (newline)
  (define compile-args (store-arg-offset fname args compile-vars))
  ;(display compile-args) (newline)
  (cons (list 'label (func-label fname)) compile-args))

(define (func-epilogue fname args vars)
  (cond
    [(equal? 'main fname) (list (list 'label (mark-epilogue fname))
                                '(halt))]
    [true (list (list 'label (mark-epilogue fname))
                (list 'sub 'SP 'SP (+ 2 (length vars)))
                '(move FP (0 SP))
                '(move RETURN-ADDR (1 SP))
                '(jump RETURN-ADDR))]))

(define (mark-epilogue fname)
  (string->symbol (string-append "EPILOGUE_" (symbol->string fname))))

(define (compile-statement fname stmt output)
  (match stmt 
    [`(print ,content) 
     (cond
       [(string? content) (cons (list 'print-string content) output)]
       [true
        (define compile-content (compile-statement fname content empty))
        (define pop-stack '((print-val (-1 SP))
                            (sub SP SP 1)))
        (append compile-content (append pop-stack output))])]
    [`(set ,id ,aexp)
     (define compile-aexp (compile-statement fname aexp empty))
     (define set-id (list (list 'move (address-of fname id) '(-1 SP))
                          '(sub SP SP 1)))
     (append compile-aexp (append set-id output))]
    [(cons 'seq stmt)
     (compile-seq-helper fname (reverse stmt) output)]
    [`(iif ,bexp ,stmt1 ,stmt2)
     (define label0 (new-label))
     (define label1 (new-label))
     (define label2 (new-label))
     (define compile-bexp (compile-statement fname bexp empty))
     (define control-flow1 (list (list 'branch '(-1 SP) label0)
                                 '(sub SP SP 1)
                                 (list 'jump label1)
                                 (list 'label label0)
                                 '(sub SP SP 1)))
     (define compile-stmt1 (compile-statement fname stmt1 empty))
     (define control-flow2 (list (list 'jump label2)
                                 (list 'label label1)))
     (define compile-stmt2 (compile-statement fname stmt2 empty))
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
     (define compile-bexp (compile-statement fname bexp empty))
     (define control-flow2 (list (list 'branch '(-1 SP) label1)
                                 '(sub SP SP 1)
                                 (list 'jump label2)
                                 (list 'label label1)
                                 '(sub SP SP 1)))
     (define compile-stmt (compile-statement fname (cons 'seq stmt) empty))
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
       [(empty? bexp2) (append (compile-statement fname bexp1 empty) output)]
       [true
        (define compile-bexp1 (compile-statement fname bexp1 empty))
        (define compile-bexp2 (compile-statement fname (cons 'and bexp2) empty))
        (define apply-op (list '(land (-2 SP) (-2 SP) (-1 SP))
                               '(sub SP SP 1)))
        (append compile-bexp1 (append compile-bexp2 (append apply-op output)))])]
    [(cons 'or bexp-lst)
     (define bexp1 (car bexp-lst))
     (define bexp2 (cdr bexp-lst))
     (cond
       [(empty? bexp2) (append (compile-statement fname bexp1 empty) output)]
       [true
        (define compile-bexp1 (compile-statement fname bexp1 empty))
        (define compile-bexp2 (compile-statement fname (cons 'or bexp2) empty))
        (define apply-op (list '(lor (-2 SP) (-2 SP) (-1 SP))
                               '(sub SP SP 1)))
        (append compile-bexp1 (append compile-bexp2 (append apply-op output)))])]
    [`(not ,bexp)
     (define compile-bexp (compile-statement fname bexp empty))
     (define apply-not (list '(lnot (-1 SP) (-1 SP))))
     (append compile-bexp (append apply-not output))]
    [(list (? (lambda(x) (hash-has-key? dispatch-table x))) exp1 exp2)
     (define op (first stmt))
     ;(display op)
     (define compile-exp1 (compile-statement fname exp1 empty))
     (define compile-exp2 (compile-statement fname exp2 empty))
     (define apply-op (list (list (hash-ref dispatch-table op) '(-2 SP) '(-2 SP) '(-1 SP))
                            '(sub SP SP 1)))
     (append compile-exp1 (append compile-exp2 (append apply-op output)))]
    [`(return ,aexp)
     (define compile-aexp (compile-statement fname aexp empty))
     (define compile-return (list '(move RETURN-VAL (-1 SP))
                                  '(sub SP SP 1)
                                  (list 'jump (mark-epilogue fname))))
     (append compile-aexp (append compile-return output))]
    [(cons f-id args)
     (define arg-num (length args))
     (define add-fun-args (add-args fname args empty))
     (define call-func (list (list 'jsr 'RETURN-ADDR (func-label f-id))))
     (define remove-fun-args (make-list arg-num '(sub SP SP 1)))
     (define add-stack-return (list '(move (0 SP) RETURN-VAL)
                                    '(add SP 1 SP)))
     (cond
       [(= arg-num (hash-ref hash-arg-num f-id))
        (append add-fun-args
                (append call-func
                        (append remove-fun-args
                                (append add-stack-return output))))]
       [true (error 'arguments-number-error)])]
    [x
     (define compiled (list (list 'move '(0 SP) (address-of fname x))
                            (list 'add 'SP 1 'SP)))
     (append compiled output)]))

;; create instructions that add function arguments to the stack
(define (add-args fname arg-lst acc)
  (cond
    [(empty? arg-lst) acc]
    [true (add-args fname
                    (cdr arg-lst)
                    (append (compile-statement fname (car arg-lst) empty) acc))]))


;; given: variable name var and function name func
;; output: _func_var
(define (underscore func var)
  (cond
    [(equal? var 'true) #t]
    [(equal? var 'false) #f]
    [(symbol? var) (string->symbol (string-append "_" (symbol->string func) "_" (symbol->string var)))]
    [true var])
  )

(define (address-of func var)
  (cond
    [(equal? var 'true) #t]
    [(equal? var 'false) #f]
    [(symbol? var) (list (underscore func var) 'FP)]
    [true var])
  )

;; create label for given function name
(define (func-label fname)
  (string->symbol (string-append "_" (symbol->string fname))))

;; given function name func and list of variable names vars
;; return list of const statements, map (underscore func var) to offset of address of the variable relative to FP
;; append that list to acc
(define (store-arg-offset func vars acc)
  (define first-offset -1)
  (define (helper func vars offset acc)
    (cond
      [(empty? vars) acc]
      [true
       (define new (list 'const (underscore func (car vars)) offset))
       (helper func (cdr vars) (sub1 offset) (cons new acc))]))
  (helper func vars first-offset acc))

;; store offset of function variables using const statmenets
;; then store value of variables in corresponding address using move
(define (store-vars func vars acc)
  (define (helper-const func vars offset acc)
    (cond
      [(empty? vars) acc]
      [true
       (define new (list 'const (underscore func (first (car vars))) offset))
       (helper-const func (cdr vars) (add1 offset) (cons new acc))]))
  (define (helper-move vars acc)
    (cond
      [(empty? vars) acc]
      [true
       (define new (list 'move (list (underscore func (first (car vars))) 'SP) (second (car vars))))
       (helper-move (cdr vars) (cons new acc))]))
  (helper-const func vars 2 (helper-move vars acc)))

(define (compile-seq-helper fname stmt output)
  (cond
    [(= 1 (length stmt)) (compile-statement fname (car stmt) output)]
    [true (define compile-first (compile-statement fname (car stmt) output))
          (compile-seq-helper fname (cdr stmt) compile-first)]))

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

;(compile-simpl example)