#lang racket
(provide run-SIMPL)
(provide run-SIMPL-F)
(provide compile-SIMPL)
(provide compile-SIMPL-F)
(provide assemble-A-PRIMPL)
(provide run-PRIMPL)

(require "assembler.rkt")
(require "compiler-function.rkt")
(require "compiler.rkt")
(require "run-PRIMPL.rkt")

; run given SIMPL program written as a list, return program output as string
(define (run-SIMPL prog-lst)
  (cond
    [(empty? prog-lst) ""]
    [true
     (with-handlers ([exn:fail? (lambda (exn) "error\n")])
       (begin (load-primpl (primplify (compile-simpl prog-lst)))
              (run-primpl))
       (get-output-string out))]))

; run given SIMPL-F program written as a list, return program output as string
(define (run-SIMPL-F prog-lst)
  (cond
    [(empty? prog-lst) ""]
    [true
     (with-handlers ([exn:fail? (lambda (exn) "error\n")])
       (begin (load-primpl (primplify (compile-simpl-func prog-lst)))
              (run-primpl))
       (get-output-string out))]))

; compile given SIMPL program written as a list, return compiled A-PRIMPL program as string
(define (compile-SIMPL prog-lst)
  (define o (open-output-string))
  (with-handlers ([exn:fail? (lambda (exn) "error\n")])
    (write (compile-simpl prog-lst) o)
    (get-output-string o))
  )

; compile given SIMPL-F program written as a list, return compiled A-PRIMPL program as string
(define (compile-SIMPL-F prog-lst)
  (define o (open-output-string))
  (with-handlers ([exn:fail? (lambda (exn) "error\n")])
    (write (compile-simpl-func prog-lst) o)
    (get-output-string o))
  )

; assemble given A-PRIMPL program written as a list, return assembled machine code (PRIPL) as string
(define (assemble-A-PRIMPL prog-lst)
  (define o (open-output-string))
  (with-handlers ([exn:fail? (lambda (exn) "error\n")])
    (write (primplify prog-lst) o)
    (get-output-string o))
  )

; run given PRIMPL program written as a list, return program output
(define (run-PRIMPL prog-lst)
  (cond
    [(empty? prog-lst) ""]
    [true
     (with-handlers ([exn:fail? (lambda (exn) "error\n")])
       (begin (load-primpl prog-lst)
              (run-primpl))
       (get-output-string out))])
  )