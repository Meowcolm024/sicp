#lang sicp

(define (make-new-machine)
    (let ((pc (make-register 'pc))
          (flag (make-register 'flag))
          (stack (make-stack))
          (the-instrcution-sequence '()))
        (let ((the-ops
                (list (list 'initialize-stack (lambda () (stack 'initialize)))))
              (register-table
                (list (list 'pc pc) (list 'flag flag))))
        (define (allocate-register name)
            (if (assoc name register-table)
                (error "Multiple")
                (set! register-table 
                    (cons (list name (make-register name)) register-table)))
            'register-allocated)
        (define (lookup-register name) 
            (let ((val (assoc name register-table)))
                (if val
                    (cadr val)
                    (error "Unknown"))))
        (define (execute)
            (let ((insts (get-contents pc)))
                (if (null? insts)
                    'done
                    (begin
                        ((instruction-execution-proc (car insts)))
                        (execute)))))
        (define (dispatch message)
            (cond ((eq? message 'start)
                    (set-contenes! pc the-instrcution-sequence)
                    (execute))
                  ((eq? message 'install-instruction-sequence)
                    (lambda (seq)
                        (set! the-instrcution-sequence seq)))
                  ((eq? message 'allocate-register)
                    allocate-register)
                  ((eq? message 'get-register)
                    lookup-register)
                  ((eq? message 'install-operations)
                    (lambda (ops) (set! the-ops (append the-ops ops))))
                  ((eq? message 'stack) stack)
                  ((eq? message 'operayions) the-ops)
                  (else (error "Unknown"))))
        dispatch)))

(define (start machine) (machine 'start))
(define (get-register-contents machine register-name)
    (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
    (set-contenes! (get-register machine register-name) value) 'done)
(define (get-register machine reg-name)
    ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
    (extract-labels
        controller-text
        (lambda (insts labels)
            (update-insts! insts labels machine)
            insts)))

(define (extract-labels text receive)
    (if (null? text)
        (receive '() '())
        (extract-labels
            (cdr text)
            (lambda (insts labels)
                (let ((next-inst (car text)))
                    (if (symbol? next-inst)
                        (receive insts 
                            (cons (make-label-entry next-inst insts) 
                                    labels))
                        (receive (cons (make-instruction next-inst) insts) 
                                        labels)))))))

(define (make-label-entry label-name insts) (cons label-name insts))

(define (update-insts! insts labels machine) 
    (let ((pc (get-register machine 'pc))
          (flag (get-register machine 'flag))
          (stack (machine 'stack))
          (ops (machine 'operations)))
        (for-each
            (lambda (inst)
                (set-instruction-execution-proc!
                    inst
                    (make-execution-procedure
                        (instruction-text inst) labels machine pc flag stack ops)))
            insts)))

(define (make-instruction text) (cons text '()))
(define (set-instruction-execution-proc! inst proc) (set-cdr! inst proc))
(define instruction-text car)
(define instruction-execution-proc cdr)

(define (lookup-label labels label-name)
    (let ((val (assoc label-name labels)))
        (if val
            (cdr val)
            (error "Undefined"))))

(define (make-execution-procedure inst labels machine pc flag stack ops)
    (cond ((eq? (car inst) 'assign) 
            (make-assign inst machine labels ops pc))
          ((eq? (car inst) 'test)
            (make-test inst machine labels ops flag pc))
          ((eq? (car inst) 'branch)
            (make-branch inst machine labels flag pc))
          ((eq? (car inst) 'goto)
            (make-goto inst machine labels pc))
          ((eq? (car inst) 'save)
            (make-save inst machine stack pc))
          ((eq? (car inst) 'restore)
            (make-restore inst machine stack pc))
          ((eq? (car inst) 'perform)
            (make-perform inst machine labels ops pc))
          (else (error "Unknown"))))

(define (make-machine register-names ops controller-text)
    (let ((machine (make-new-machine)))
        (for-each
            (lambda (register-name)
                ((machine 'allocate-register) register-name))
            register-names)
        ((machine 'install-operations) ops)
        ((machine 'install-instruction-sequence)
            (assemble controller-text machine))
        machine))

(define (make-register name)
    (let ((contents '*unassigned*))
        (define (dispatch message)
            (cond ((eq? message 'get) contents)
                  ((eq? message 'set)
                    (lambda (value) (set! contents value)))
                  (else (error "Unknown"))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contenes! register value) ((register 'set) value))

(define (make-stack)
    (let ((s '()))
        (define (push x) (set! s (cons x s)))
        (define (pop)
            (if (null? s)
                (error "Empty")
                (let ((top (car s)))
                    (set! s (cdr s))
                    top)))
        (define (initialize)
            (set! s '())
            'done)
        (define (dispatch message)
            (cond ((eq? message 'push) push)
                  ((eq? message 'pop) pop)
                  ((eq? message 'initialize) initialize)
                  (else (error "Unknown"))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

;;; (get-register-contents!)
;;; (start)

(define gcd-machine
    (make-machine
        '(a b t)
        (list (list 'rem remainder) (list '= =))
        '(test-b (test (op =) (reg b) (const 0))
                 (branch (label gcd-done))
                 (assign t (op rem) (reg a) (reg b))
                 (assign a (reg b))
                 (assign b (reg t))
                 (goto (label test-b))
                 gcd-done)))
