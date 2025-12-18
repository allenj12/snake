#!chezscheme
(library (scrutiny scrutiny)
    (export test-compile
            define-unit
            test-let
            test-if
            test-execute
            any?
            not-null?
            not-zero?
            natural-number?
            natural-fxnumber?
            gen-natural-fxnumber
            whole-number?
            whole-fxnumber?
            gen-whole-fxnumber
            ascii-char?
            gen-ascii-char
            alpha-char?
            gen-alpha-char
            lower-char?
            gen-lower-char
            upper-char?
            gen-upper-char
            numeric-char?
            gen-numeric-char
            alpha-numeric-char?
            gen-alpha-numeric-char
            is-all?)
    (import (chezscheme))

(meta define test #f)

(define-syntax test-compile
    (lambda arg
        (set! test #t)
        #'(void)))

(define-syntax define-unit
    (lambda (stx)
        (syntax-case stx ()
            [(_ name iterations ((var validator generator) ... return-validator) body ...)
             ;TODO: add syntax violation here if iterations is not a literal number
             (if test 
                 #`(begin
                       (define name
                           (lambda (var ...) 
                               (assert (validator var)) ...
                               ((lambda ()
                                   (define res ((lambda () body ...)))
                                   (assert (return-validator res))
                                   res))))
                       (for-each (lambda (x) (name (generator) ...)) (iota iterations)))
                 #'(define name (lambda (var ...) body ...)))]
            [(_ name ((var validator) ... return-validator) body ...)
             #`(define name
                       #,(if test
                             #'(lambda (var ...) 
                                   (assert (validator var)) ... 
                                   ((lambda ()
                                       (define res ((lambda () body ...)))
                                       (assert (return-validator res))
                                       res)))
                             #'(lambda (var ...) body ...)))])))

(define-syntax test-let
    (lambda (stx)
        (syntax-case stx ()
            [(_ ([var normal-val test-val] ...) body ...)
              (if test
                  #'(let* ([var test-val] ...) body ...)
                  #'(let* ([var normal-val] ...) body ...))])))

(define-syntax test-if
    (lambda (stx)
        (syntax-case stx ()
            [(_ test-body run-body)
             (if test
                 #'test-body
                 #'run-body)])))

(define-syntax test-execute
    (lambda (stx)
        (syntax-case stx ()
            [(_ body ...)
             (if test
                 #'(begin body ...)
                 #'(void))])))

(define any? (lambda (n) #t))

(define not-null?
    (lambda (n) (not (null? n))))

(define not-zero?
    (lambda (n) (not (zero? n))))

(define natural-number?
    (lambda (n)
        (and (integer? n) (fx> n 0))))

(define natural-fxnumber?
    (lambda (n)
        (and (fixnum? n) (fx> n 0))))

(define gen-natural-fxnumber
    (lambda ()
        (fx1+ (random (most-positive-fixnum)))))

(define whole-number?
    (lambda (n)
        (and (integer? n) (fx>= n 0))))

(define whole-fxnumber?
    (lambda (n)
        (and (integer? n) (fx>= n 0) (fixnum? n))))

(define gen-whole-fxnumber
    (lambda ()
        (random (most-positive-fixnum))))

(define ascii-char?
    (lambda (c)
        (and (char? c) (fx< (char->integer c) 128))))

(define gen-ascii-char
    (lambda ()
        (integer->char (random 128))))

(define lower-char?
    (lambda (c)
        (let ([n (char->integer c)])
            (and (char? c) (and (fx> n 64) (fx< n 91))))))

(define gen-lower-char
    (lambda ()
        (integer->char (fx+ 65 (random 26)))))

(define upper-char?
    (lambda (c)
        (let ([n (char->integer c)])
            (and (char? c) (and (fx> n 96) (fx< n 123))))))

(define gen-upper-char
    (lambda ()
        (integer->char (fx+ 97 (random 26)))))

(define alpha-char? 
    (lambda (c)
        (let ([n (char->integer c)])
            (and (char? c) (or (and (fx> n 64) (fx< n 91))
                               (and (fx> n 96) (fx< n 123)))))))

(define gen-alpha-char
    (lambda ()
        (if (zero? (random 2))
            (gen-lower-char)
            (gen-upper-char))))

(define numeric-char?
    (lambda (c)
        (let ([n (char->integer c)])
            (and (char? c) (and (fx> n 47) (fx< n 58))))))

(define gen-numeric-char
    (lambda ()
        (integer->char (fx+ 48 (random 10)))))

(define alpha-numeric-char?
    (lambda (c)
        (let ([n (char->integer c)])
            (and (char? c) (or (and (fx> n 64) (fx< n 91))
                               (and (fx> n 96) (fx< n 123))
                               (and (fx> n 47) (fx< n 58)))))))

(define gen-alpha-numeric-char
    (lambda ()
        (case (random 2)
            (0 (gen-alpha-char))
            (1 (gen-numeric-char)))))

(define is-all?
    (lambda preds
        (lambda (x) (andmap (lambda (p) (p x)) preds))))
)