;;;; package.lisp

(cl:in-package cl-user)


(defpackage "https://github.com/g000001/srfi-71"
  (:use)
  (:export letrec*
           letrec
           let*
           let
           uncons
           uncons-2
           uncons-3
           uncons-4
           uncons-cons
           unlist
           unvector
           values->list
           values->vector))


(defpackage "https://github.com/g000001/srfi-71#internals"
  (:use "https://github.com/g000001/srfi-71"
        cl
        fiveam
        mbe)
  (:shadowing-import-from "https://github.com/g000001/srfi-71"
                          let
                          let*)
  (:shadow t))


;;; *EOF*
