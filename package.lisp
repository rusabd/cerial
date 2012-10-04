;;;; package.lisp

(defpackage #:cerial
  (:use #:cl #:annot.class))


#+(or linux unix)
(defpackage #:unistd
  (:use #:cl 
	#:annot.class
	#+(not sbcl) #:cffi)
  (:shadow #:cl #:close #:open #:write #:read))
   

#+(and sbcl (or linux unix)) (do-external-symbols (s (find-package :unistd))
			       (export s #:unistd))

