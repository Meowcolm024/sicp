#lang sicp

(define make-new-machine '())
(define assemble '())

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
