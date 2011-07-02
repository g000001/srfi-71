(cl:in-package :srfi-71-internal)

;; adopted from kawa 1.9 (mit licence)
(define-syntax r5rs-letrec
  (syntax-rules ()
    ((letrec bindings . body)
     (%letrec1 () bindings . body))))

(define-syntax %letrec1
  (syntax-rules (|::|)
    ((%letrec1 done ((x |::| type init) . bindings) . body)
     (%letrec1 ((x |::| type '#:undefined) . done) bindings (set! x init) . body))
    ((%letrec1 done ((x init) . bindings) . body)
     (%letrec1 ((x '#:undefined) . done) bindings (set! x init) . body))
    ((%letrec1 done () . body)
     (srfi-86-internal::let done . body))))


(let (((values x y z) (values 1 2 3)))
  (list x y z))


(let ((x 3))
  (list x))


(let* ((x 3)
       (x 3))
  (list x))


(let ((car-x cdr-x (uncons '(1 . 2))))
  (list  car-x cdr-x))


(|I:LET| "form1"
         :FALSE
         NIL
         ((CAR-X T1))
         ((LIST CAR-X CDR-X)) NIL ((UNCONS '(1 . 2)) (T1)) (VALUES CDR-X))


(let (((values x1 x2 . x2+) (values 1 2 3)))
  (cons x1 (cons x2 x2+)))

(|I:LET| "bs"
         :FALSE
         NIL
         NIL
         ((CONS X1 (CONS X2 X2+)))
         (((VALUES X1 X2 . X2+)
           (VALUES 1 2 3))))

(|I:LET| "form1+" :FALSE NIL NIL
         ((CONS X1 (CONS X2 X2+)))
         NIL
         ((VALUES 1 2 3) NIL)
         (VALUES X1 X2 . X2+))


(|I:LET| "form1+" :FALSE NIL
         ((X1 T1))
         ((CONS X1 (CONS X2 X2+)))
         NIL
         ((VALUES 1 2 3) (T1))
         (VALUES X2 . X2+))




