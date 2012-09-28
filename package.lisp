;;;; package.lisp

(defpackage #:cerial
  (:use #:cl))


#+(or linux unix)
(defpackage #:unistd
  (:use #:cl 
	#:annot.class
	#+(not sbcl) #:cffi)
  (:shadow #:cl #:close #:open))
   

#+(and sbcl (or linux unix)) (do-external-symbols (s (find-package :unistd))
			       (export s #:unistd))

