#lang web-server/insta

(require "assembler.rkt")
(require "compiler-function.rkt")
(require "compiler.rkt")
(require "run-PRIMPL.rkt")

; compile given program written as a list
(define (compile prog-lst)
  (cond
    [(empty? prog-lst) ""]
    [true
     (with-handlers ([exn:fail? (lambda (exn) "error\n")])
       (begin (load-primpl (primplify (compile-simpl prog-lst)))
              (run-primpl))
       (get-output-string out))]))

; give a string, output html x-expression that display that string. Output is list
(define (render-output output-str)
  `(body (h1 ,output-str)))

; given a string that represents a list, like "(1 2 3)". Return the list it represents, like '(1 2 3)
; return the string if can't be converted to list
(define (str->lst str)
  (cond
    [(equal? str "")
     empty]
    [true
     (with-handlers ([exn:fail? (lambda (exn) str)])
       (with-input-from-string str
         read))
     ]))

; takes a request, return input in the input area (such as textarea) with given name
; name: symbol
; output: string
(define (get-input request name)
  (define req (request-bindings request))
  (cond
    [(exists-binding? name req)
     (extract-binding/single name req)]
    [true ""]))

(define (start request)
  (define input-str (get-input request 'input))
  (define input-lst (str->lst input-str))
  (define output (compile input-lst))
  (response/xexpr
   `(html
     [head (title "Run SIMPL")]
     [h1 "Run SIMPL Code"]
     [form
      (textarea ((name "input") (rows "20") (cols "75") (style "font-size: 20pt"))
                ,input-str)
      (input ((type "submit")))]
     ,(render-output output)
     )))
