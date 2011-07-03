(cl:in-package :srfi-71-internal)

(define-syntax r5rs-letrec
  (syntax-rules ()
    ((letrec bindings . body)
     (%letrec1 () bindings . body))))

(define-syntax %letrec1
  (syntax-rules (|::|)
    ((%letrec1 done ((x |::| type init) . bindings) . body)
     (%letrec1 ((x |::| type '#:undefined) . done) bindings (setq x init) . body))
    ((%letrec1 done ((x init) . bindings) . body)
     (%letrec1 ((x '#:undefined) . done) bindings (setq x init) . body))
    ((%letrec1 done () . body)
     (srfi-86-internal::let done . body))))

(defun to-proper-lambda-list (list)
  (typecase list
    (list (if (tailp () list)
              list
              (let ((last (last list)))
                `(,@(butlast list)
                      ,(car last)
                    cl:&rest
                    ,(cdr last)))))
    (symbol `(cl:&rest ,list))))

(defmacro scheme-lambda (args &rest body)
  `(lambda ,(to-proper-lambda-list args)
     ,@body))