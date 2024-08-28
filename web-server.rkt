#lang web-server/insta

(require "execution.rkt")

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


(struct input (execoption textarea))
; takes a request, a input struct
; exec-option: execution type as string, such as Run SIMPL, Run SIMPL-F, Compile SIMPL, etc.
; textarea: input in the input textarea 
; name: symbol
; output: input struct
(define (get-input request)
  (define req (request-bindings request))
  (define exec-op
    (cond
    [(exists-binding? 'options req)
     (extract-binding/single 'options req)]
    [true "Run SIMPL"]))
  (define text-area
    (cond
    [(exists-binding? 'input req)
     (extract-binding/single 'input req)]
    [true ""]))
  (input exec-op text-area))

; compile/assemble/execute text-in based on given exec-op
; exec-op: string
; text-in: string
; return str
(define (generate-output exec-op text-in)
  (define text-input-lst (str->lst text-in))
  (cond
    [(equal? exec-op "Run SIMPL")
     (run-SIMPL text-input-lst)]
    [(equal? exec-op "Run SIMPL-F")
     (run-SIMPL-F text-input-lst)]
    [(equal? exec-op "Compile SIMPL")
     (compile-SIMPL text-input-lst)]
    [(equal? exec-op "Compile SIMPL-F")
     (compile-SIMPL-F text-input-lst)]
    [(equal? exec-op "Assemble A-PRIMPL")
     (assemble-A-PRIMPL text-input-lst)]
    [(equal? exec-op "Run PRIMPL")
     (run-PRIMPL text-input-lst)]
    [true
     ""]
    ))

(define (start request)
  (define input (get-input request))    ; get input struct containing input in text area and execution option 
  (define text-input-str (input-textarea (get-input request)))    ; get input in the text area
  (define exec-option (input-execoption (get-input request)))   ; Get execute option: Run SIMPL, Run SIMPL-F, Compile SIMPL, etc.
  (define output (generate-output exec-option text-input-str))    ; generate output based on exec-option and text input
  (response/xexpr
   `(html
     [head (title ,exec-option)]
     [h1 ,exec-option]
     [form
      (textarea ((name "input") (rows "20") (cols "75") (style "font-size: 20pt"))
                ,text-input-str)
      (select ((name "options"))
              (option ((value "Run SIMPL")) "Run SIMPL")
              (option ((value "Run SIMPL-F")) "Run SIMPL-F")
              (option ((value "Compile SIMPL")) "Compile SIMPL to A-PRIMPL")
              (option ((value "Compile SIMPL-F")) "Compile SIMPL-F to A-PRIMPL")
              (option ((value "Assemble A-PRIMPL")) "Assemble A-PRIMPL to PRIMPL")
              (option ((value "Run PRIMPL")) "Run PRIMPL"))
      (input ((type "submit")))]
     ,(render-output output)
     )))
