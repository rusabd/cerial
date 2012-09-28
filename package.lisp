;;;; package.lisp

(defpackage #:cerial
  (:use #:cl))

(defpackage #:unistd
  (:use #:cl #:cffi)
  (:shadow #:cl #:close #:open))
