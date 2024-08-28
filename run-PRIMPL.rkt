#lang racket

(provide load-primpl)
(provide run-primpl)
(provide out)

;; This program runs PRIMPL code
;; How to use:
;    Step 1. call (load-primpl inst-lst), where inst-lst is a list of instructions
;    Step 2. call (run-primpl)
;; One possible example of inst-lst is the following:
(define example
  '((add (4) (2) (3))
    (print-val (4))
    1
    2
    0
    0))

;; output stream
(define out (open-output-string))

;; size of memory of the machine it simulates
(define MEM-SIZE 10000)

(define mem (make-vector MEM-SIZE 0))  ; memory
(define pc 0) ; program counter
(define halted? false)

;; get value at memory address
(define (mem-get i)
  (vector-ref mem i))

;; set value at memory address
(define (mem-set! i newv)
  (vector-set! mem i newv))

;; takes a list of PRIMPL instructions, initialize halted? flag, pc, and memory. Load instructions to memory
;; return void
(define (load-primpl prog-lst)
  (set! out (open-output-string))
  (set! pc 0)
  (set! halted? false)
  (vector-fill! mem 0)
  (for [(i MEM-SIZE)]
    (define instruction empty)
    (cond
      [(empty? prog-lst)
       (set! instruction 0)]
      [true
       (set! instruction (car prog-lst))
       (set! prog-lst (cdr prog-lst))])
    (mem-set! i instruction)))

;; run the program loaded in memory
;; return void
(define (run-primpl)
  (let loop ()
    (if halted?
        (void)
        (begin (fetch-execute-once) (loop)))))

;; fetch and execute one instruction, then increment pc
;; return void
(define (fetch-execute-once)
  (define instruction (mem-get pc))
  (cond
    [(list? instruction)
     (set! pc (add1 pc))
     (dispatch-inst instruction)]  ;; call function to execute instruction
    [true
     (set! halted? #t)]))

;; look for instruction in dispatch table and execute it
;; return void
(define (dispatch-inst inst)
  (cond
    [(empty? inst) (error "bad instruction")]
    [true (apply
           (hash-ref dispatch-table (car inst) (lambda() (error "bad instruction")))
           (cdr inst))]))

;; decode operand as immediate (like 1) or memory reference (like (1), representing value at mem addr 1)
;; returns the value
(define (get-op-imm-or-mem op)
  (match op
    [(or (? number? v) (? boolean? v)) v]   ; immediate
    [`(,i) (mem-get i)]    ; indirect
    [`(,i (,j)) (mem-get (+ i (mem-get j)))]    ; indexed
    [x (error "Bad operand")]))

;; set value to given location, indirect or indexed
;; return void
(define (set-dest! op v)
  (match op
    [`(,i) (mem-set! i v)] ; indirect
    [`(,i (,j)) (mem-set! (+ i (mem-get j)) v)] ; indexed
    [x (error "Bad destination")]))

;; print string
(define (print-string s)
  (write s out))

;; print value (immediate or value at memory loc)
(define (print-val op)
  (define val (get-op-imm-or-mem op))
  (write val out))

;; binary number operation instruction with parameters src1 and src2, store result in dest
(define ((bin-num-op op) dest src1 src2)
  (define opnd1 (get-op-imm-or-mem src1))
  (define opnd2 (get-op-imm-or-mem src2))
  (cond
    [(not (number? opnd1)) (error "First operand not number: ~a ~a" opnd1 opnd2)]
    [(not (number? opnd2)) (error "Second operand not number: ~a ~a" opnd1 opnd2)]
    [true (set-dest! dest (op opnd1 opnd2))]))

;; binary number operations
(define add (bin-num-op +))
(define sub (bin-num-op -))
(define mul (bin-num-op *))
(define div (bin-num-op quotient))
(define mod (bin-num-op modulo))
(define equal (bin-num-op equal?))
(define not-equal (bin-num-op (negate equal?)))
(define gt (bin-num-op >))
(define ge (bin-num-op >=))
(define lt (bin-num-op <))
(define le (bin-num-op <=))

;; binary logical operation instruction with parameters src1 and src2, store result in dest
(define ((bin-logical-op op) dest src1 src2)
  (define opnd1 (get-op-imm-or-mem src1))
  (define opnd2 (get-op-imm-or-mem src2))
  (cond
    [(not (boolean? opnd1)) (error "First operand not bool: ~a ~a" opnd1 opnd2)]
    [(not (boolean? opnd2)) (error "Second operand not bool: ~a ~a" opnd1 opnd2)]
    [true (set-dest! dest (op opnd1 opnd2))]))

;; bool operations
(define land (bin-logical-op (lambda (b1 b2) (and b1 b2))))
(define lor (bin-logical-op (lambda (b1 b2) (or b1 b2))))
(define (lnot dest src)
  (define opnd (get-op-imm-or-mem src))
  (cond
    [(not (boolean? opnd)) (error "Operand not bool: ~a" opnd)]
    [true (set-dest! dest (not opnd))]))

;; other operations
(define (move dest src)    ; set value corresponding to src to addres dest
  (set-dest! dest (get-op-imm-or-mem src)))

(define (jump loc)    ; move pc to given location
  (define target (get-op-imm-or-mem loc))
  (set! pc target))

(define (jsr dest loc)    ; move pc to given location, store old pc at dest
  (define target (get-op-imm-or-mem loc))
  (set-dest! dest pc)
  (set! pc target))

(define (branch opnd loc)    ; move pc to given location if condition opnd is true
  (define target (get-op-imm-or-mem loc))
  (define condition (get-op-imm-or-mem opnd))
  (if condition (set! pc target) (void)))

;; dispatch table
(define dispatch-table
  (hash
   'print-val print-val
   'print-string print-string
   'add add
   'sub sub
   'mul mul
   'div div
   'mod mod
   'equal equal
   'not-equal not-equal
   'gt gt
   'ge ge
   'lt lt
   'le le
   'land land
   'lor lor
   'lnot lnot
   'move move
   'jump jump
   'jsr jsr
   'branch branch))

(load-primpl '())

(run-primpl)