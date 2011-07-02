;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-71
  (:use)
  (:export
   :letrec*
   :letrec
   :let*
   :let
   :uncons
   :uncons-2
   :uncons-3
   :uncons-4
   :uncons-cons
   :unlist
   :unvector
   :values->list
   :values->vector))

(defpackage :srfi-71-internal
  (:use :srfi-71 :cl :fiveam
        :mbe)
  (:shadowing-import-from :srfi-71 :let :let*))

