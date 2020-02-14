;;;; srfi-71.asd

(cl:in-package :asdf)


(defsystem :srfi-71
  :version "20200215"
  :description "SRFI 71 for CL: Extended LET-syntax for multiple values"
  :long-description "SRFI 71 for CL: Extended LET-syntax for multiple values
https://srfi.schemers.org/srfi-71"
  :author "Sebastian Egner"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (:mbe :srfi-5)
  :components ((:file "package")
               (:file "utils")
               (:file "srfi-71")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-71))))
  (let ((name "https://github.com/g000001/srfi-71")
        (nickname :srfi-71))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-71))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-71#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-71)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
