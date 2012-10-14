;;;; cerial.asd

(asdf:defsystem #:cerial
  :serial t
  :description "Describe cerial here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-annot 
	       #+(not sbcl) #:cffi
	       #:alexandria)
  :components ((:file "package")
	       #+(or linux unix) (:file "unistd")
	       #+(or mswindows windows) (:file "win32")
               (:file "serial-base")
	       #+(or linux unix) (:file "serial-posix")
	       #+(or mswindows windows) (:file "serial-win32")))

