(in-package #:unistd)

(annot:enable-annot-syntax)

(cffi:define-foreign-library libc
  (:unix (:or "libc.so.6" "libc.so.5" "libc.so"))
  (t (:default "libc.so")))
 
(cffi:use-foreign-library libc)

@export
(defconstant B0 #o0) 
@export
(defconstant B50 #o1)
@export
(defconstant B75 #o2)
@export
(defconstant B110 #o3)
@export
(defconstant B134 #o4)
@export
(defconstant B150 #o5)
@export
(defconstant B200 #o6)
@export
(defconstant B300 #o7)
@export
(defconstant B600 #o10)
@export
(defconstant B1200 #o11)
@export
(defconstant B1800 #o12)
@export
(defconstant B2400 #o13)
@export
(defconstant B4800 #o14)
@export
(defconstant B9600 #o15)
@export
(defconstant B19200 #o16)
@export
(defconstant B38400 #o17)
@export
(defconstant B57600 #o10001)
@export
(defconstant B115200 #o10002)
@export
(defconstant B230400 #o10003)
@export
(defconstant B460800 #o10004)
@export
(defconstant B500000 #o10005)
@export
(defconstant B576000 #o10006)
@export
(defconstant B921600 #o10007)
@export
(defconstant B1000000 #o10010)
@export
(defconstant B1152000 #o10011)
@export
(defconstant B1500000 #o10012)
@export
(defconstant B2000000 #o10013)
@export
(defconstant B2500000 #o10014)
@export
(defconstant B3000000 #o10015)
@export
(defconstant B3500000 #o10016)
@export
(defconstant B4000000 #o10017)

@export
(defvar ONOCTTY #o00400)	     

@export
(defvar ONONBLOCK #o04000)

@export
(defvar ORDWR #o00002)

@export
(defvar FSETFL #o00004)


@export
(defun close (fd)
  "The close() function shall deallocate the file descriptor indicated by fildes."
  (foreign-funcall "close" :int fd :int))

@export
(defun open (filename flags &optional mode)
  "The open function creates and returns a new file descriptor for the file named by filename. Initially, the file position indicator for the file is at the beginning of the file. The argument mode is used only when a file is created, but it doesn't hurt to supply the argument in any case."
  (with-foreign-string (fn filename)
    (foreign-funcall "open" :string fn :int flags :int (or mode 0) :int)))


(defconstant +NCCS+ 32)

(cffi:defctype tcflag_t :uint)

(cffi:defctype cc_t :uchar)

(defmacro make-foreign-termios-struct ()
  `(cffi:defcstruct ftermios
     (c_iflag tcflag_t)
     (c_oflag tcflag_t)
     (c_cflag tcflag_t)
     (c_lflag tcflag_t)
     (c_cc cc_t :count ,+NCCS+)))

(make-foreign-termios-struct)


(defclass <termios> ()
  ((iflag :initform 0 :initarg :iflag :accessor iflag)
   (oflag :initform 0 :initarg :oflag :accessor oflag)
   (cflag :initform 0 :initarg :cflag :accessor cflag)
   (lflag :initform 0 :initarg :lflag :accessor lflag)
   (cc :initform () :initarg :cc :accessor cc)))

(defun make-termios (iflag oflag cflag lflag cc)
  (let ((cc 
	 (loop for i from 0 below +NCCS+
	    collect (mem-aref cc 'cc_t i))))
    (make-instance '<termios> :iflag iflag :oflag oflag :cflag cflag :lflag lflag :cc cc)))

@export
(defun tcgetattr (fd)
  (with-foreign-object (ptr 'ftermios)
    ;; TODO Error handling!!
    (print (foreign-funcall "tcgetattr" :int fd ftermios ptr :int))
    (with-foreign-slots  ((c_iflag c_oflag c_cflag c_lflag c_cc) ptr ftermios)
      (make-termios c_iflag c_oflag c_cflag c_lflag c_cc))))

@export
(defun tcsetattr (fd termios &optional actions)
  (with-foreign-object (ptr 'ftermios)
    (setf (foreign-slot-value ptr 'ftermios 'c_iflag) (iflag termios)
	  (foreign-slot-value ptr 'ftermios 'c_oflag) (oflag termios)
	  (foreign-slot-value ptr 'ftermios 'c_cflag) (cflag termios)
	  (foreign-slot-value ptr 'ftermios 'c_lflag) (lflag termios))
    (with-foreign-slots ((c_cc) ptr ftermios)
      (loop for i below +NCCS+
	 for el in (cc termios)
	 do (setf (mem-aref c_cc 'cc_t i) el)))
    ;; TODO Error handling!!
    (foreign-funcall "tcsetattr" :int fd :int (or actions 0) ftermios ptr :int)))




#+xxx(@export
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
(defvar PARMRK sb-posix:PARMRK))


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
