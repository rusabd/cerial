;;;; cerial.asd

(asdf:defsystem #:cerial
  :serial t
  :description "Describe cerial here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-annot 
	       #+(not sbcl) #:cffi)
  :components ((:file "package")
	       #+(or linux unix) (:file "unistd")
               (:file "serial-base")
	       #+(or linux unix) (:file "serial-posix")
	       #+windows (:file "serial-win32")))

