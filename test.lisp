(cl:in-package "https://github.com/g000001/srfi-71#internals")


(def-suite* srfi-71)


(test let
  (is (= (+ 3 4)
         (let ((x 3)
               (y 4))
           (+ x y))))
  (is (= (+ 3 3)
         (let* ((x 2)
                (x 3))
           (+ x x))))
  ;; values
  (is (equal (let (((values x1 x2 . x2+) (values 1 2 3)))
               (cons x1 (cons x2 x2+)))
             '(1 2 3)))
  (is (equal (let ((car cdr (uncons '(1 2))))
               (cons car cdr))
             '(1 2)))
  (is (equal (let (((values car cdr) (uncons '(1 2))))
               (cons car cdr))
             '(1 2)))
  (is (equal (let (((values . args) (unlist '(1 2 3 4 5))))
               args)
             '(1 2 3 4 5))))


;;; *EOF*
