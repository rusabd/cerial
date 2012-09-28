;;;; cerial.asd

(asdf:defsystem #:cerial
  :serial t
  :description "Describe cerial here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-annot #:cffi)
  :components ((:file "package")
	       (:file "unistd")
               (:file "serial-base")
	       (:file "serial-posix")
	       (:file "serial-win32")))

