;;;; srfi-71.asd

(cl:in-package :asdf)

(defsystem :srfi-71
  :serial t
  :depends-on (:mbe :srfi-5)
  :components ((:file "package")
               (:file "srfi-71")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-71))))
  (load-system :srfi-71)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-71-internal :srfi-71))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

