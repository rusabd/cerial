(in-package #:posix)
(annot:enable-annot-syntax)

(cffi:define-foreign-library libc
  (:unix (:or "libc.so.6" "libc.so.5" "libc.so"))
  (t (:default "libc.so")))
 
(cffi:use-foreign-library libc)
     
#+xxx
((defconstant B0 #o0) 
(defconstant B50 #o1)
(defconstant B75 #o2)
(defconstant B110 #o3)
(defconstant B134 #o4)
(defconstant B150 #o5)
(defconstant B200 #o6)
(defconstant B300 #o7)
(defconstant B600 #o10)
(defconstant B1200 #o11)
(defconstant B1800 #o12)
(defconstant B2400 #o13)
(defconstant B4800 #o14)
(defconstant B9600 #o15)
(defconstant B19200 #o16)
(defconstant B38400 #o17)
(defconstant B57600 #o10001)
(defconstant B115200 #o10002)
(defconstant B230400 #o10003)
(defconstant B460800 #o10004)
(defconstant B500000 #o10005)
(defconstant B576000 #o10006)
(defconstant B921600 #o10007)
(defconstant B1000000 #o10010)
(defconstant B1152000 #o10011)
(defconstant B1500000 #o10012)
(defconstant B2000000 #o10013)
(defconstant B2500000 #o10014)
(defconstant B3000000 #o10015)
(defconstant B3500000 #o10016)
(defconstant B4000000 #o10017))


@export
(defvar ONOCTTY sb-posix:O-NOCTTY)	     

@export
(defvar ONONBLOCK sb-posix:O-NONBLOCK)

@export
(defvar ORDWR sb-posix:O-RDWR)

@export
(defvar FSETFL sb-posix:F-SETFL)

@export
(defun fopen (pathname flags &optional mode)
  (if mode
      (sb-posix:open pathname flags mode)
      (sb-posix:open pathname flags)))

(cffi:defcfun ("close" fclose) :int (fd :int))

(defun foo ()
  (let ((loadavg (cffi:foreign-alloc :double :count 3)))
    (getloadavg loadavg 3)
    (prog1
	(format nil "~{~,2F~^ ~}" (loop for i from 0 to 2
				     collect (cffi:mem-aref loadavg :double i)))
      (cffi:foreign-free loadavg))))


@export
(defun fcntl (fd cmd &optional arg)
  (if arg
      (sb-posix:fcntl fd cmd arg)
      (sb-posix:fcntl fd cmd)))

@export
(defun tcgetattr (fd &optional termios)
  (if termios
      (sb-posix:tcgetattr fd termios)
      (sb-posix:tcgetattr fd)))

@export
(defun cflag (object)
  (sb-posix:termios-cflag object))

@export
(defun iflag (object)
  (sb-posix:termios-iflag object))

@export
(defun lflag (object)
  (sb-posix:termios-lflag object))

@export
(defun oflag (object)
  (sb-posix:termios-oflag object))

@export
(defvar CLOCAL sb-posix:CLOCAL)

@export
(defvar CREAD sb-posix:CREAD)

@export
(defvar ICANON sb-posix:ICANON)

@export
(defvar ECHO sb-posix:ECHO)

@export
(defvar ECHOE sb-posix:ECHOE)

@export
(defvar ECHOK sb-posix:ECHOK)

@export
(defvar ECHONL sb-posix:ECHONL)

@export
(defvar ISIG sb-posix:ISIG)

@export
(defvar IEXTEN sb-posix:IEXTEN)

@export
(defvar OPOST sb-posix:OPOST)

@export
(defvar INLCR sb-posix:INLCR)

@export
(defvar IGNCR sb-posix:IGNCR)

@export
(defvar ICRNL sb-posix:ICRNL)

@export
(defvar IGNBRK sb-posix:IGNBRK)

@export
(defvar PARMRK sb-posix:PARMRK)

(defclass termios ()
  ((cflag :initform 0 :initarg :clfag :accessor cflag)
   (iflag :initform 0 :initarg :ilfag :accessor iflag)
   (oflag :initform 0 :initarg :olfag :accessor oflag)
   (lflag :initform 0 :initarg :llfag :accessor lflag)))

#+xxx(defun cfsetispeed (speed &optional termios)
  (sb-posix:cfsetispeed speed termios))

#+xxx(defun cfsetospeed (speed &optional termios) 
  (sb-posix:cfsetospeed speed termios))

#+xxx(defun fcntl (fd cmd &optional arg)
  (sb-posix:fcntl fd cmd arg))

#+xxx(defun tcflush (fd queue-selector)
  (sb-posix:tcflush fd queue-selector))

#+xxx(defun tcsetattr (fd actions termios)
  (sb-posix:tcsetattr fd actions termios))
