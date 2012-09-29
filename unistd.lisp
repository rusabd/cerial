(in-package #:unistd)

(annot:enable-annot-syntax)

(cffi:define-foreign-library libc
  (:unix (:or "libc.so.6" "libc.so.5" "libc.so"))
  (t (:default "libc.so")))
 
(cffi:use-foreign-library libc)


;;;
;;; Constants, flgas, etc...
;;;

@export (defconstant CLOCAL    #o0004000)
@export (defconstant CREAD     #o0000200)
@export (defconstant ECHO      #o0000010)
@export (defconstant ECHOE     #o0000020)
@export (defconstant ECHOK     #o0000040)
@export (defconstant ECHONL    #o0000100)
@export (defconstant ISIG      #o0000001)
@export (defconstant ICANON    #o0000002)
@export (defconstant INLCR     #o0000100)
@export (defconstant IGNCR     #o0000200)
@export (defconstant ICRNL     #o0000400)
@export (defconstant IEXTEN    #o0100000)
@export (defconstant IGNBRK    #o0000001)
@export (defconstant OPOST     #o0000001)
@export (defconstant PARMRK    #o0000010)
@export (defconstant B0        #o0000000) 
@export (defconstant B50       #o0000001)
@export (defconstant B75       #o0000002)
@export (defconstant B110      #o0000003)
@export (defconstant B134      #o0000004)
@export (defconstant B150      #o0000005)
@export (defconstant B200      #o0000006)
@export (defconstant B300      #o0000007)
@export (defconstant B600      #o0000010)
@export (defconstant B1200     #o0000011)
@export (defconstant B1800     #o0000012)
@export (defconstant B2400     #o0000013)
@export (defconstant B4800     #o0000014)
@export (defconstant B9600     #o0000015)
@export (defconstant B19200    #o0000016)
@export (defconstant B38400    #o0000017)
@export (defconstant B57600    #o0010001)
@export (defconstant B115200   #o0010002)
@export (defconstant B230400   #o0010003)
@export (defconstant B460800   #o0010004)
@export (defconstant B500000   #o0010005)
@export (defconstant B576000   #o0010006)
@export (defconstant B921600   #o0010007)
@export (defconstant B1000000  #o0010010)
@export (defconstant B1152000  #o0010011)
@export (defconstant B1500000  #o0010012)
@export (defconstant B2000000  #o0010013)
@export (defconstant B2500000  #o0010014)
@export (defconstant B3000000  #o0010015)
@export (defconstant B3500000  #o0010016)
@export (defconstant B4000000  #o0010017)
@export (defconstant ONOCTTY   #o0000400)	     
@export (defconstant ONONBLOCK #o0004000)
@export (defconstant ORDWR     #o0000002)
@export (defconstant OWRONLY   #o0000001)
@export (defconstant ORDONLY   #o0000000)
@export (defconstant OTRUNC    #o0001000)
@export (defconstant OCREAT    #o0000100)
@export (defconstant SIRUSR    #o0000400)
@export (defconstant SIWUSR    #o0000200)
@export (defconstant SIXUSR    #o0000100)
@export (defconstant SIRGRP    #o0000040)
@export (defconstant SIWGRP    #o0000020)
@export (defconstant SIXGRP    #o0000010)
@export (defconstant SIROTH    #o0000004)
@export (defconstant SIWOTH    #o0000002)
@export (defconstant SIXOTH    #o0000001)
@export (defconstant FSETFL    #o0000004)
@export (defconstant CS5       #o0000000)
@export (defconstant CS6       #o0000020)
@export (defconstant CS7       #o0000040)
@export (defconstant CS8       #o0000060)
@export (defconstant CSTOPB    #o0000100)
@export (defconstant VMIN      #o0000006)
@export (defconstant VTIME     #o0000005)
@export (defconstant TCSANOW   #o0000000)

(defconstant +FD-SETSIZE+ 256)
(defconstant +NBBY+ 8)
(defconstant +NFDBITS+ (* +NBBY+ (foreign-type-size :ulong)))
(defconstant +NCCS+ 32)

;;;
;;; Some other cruft
;;;

(defcvar "errno" :int)

(defun sizeof (obj)
  (foreign-funcall "sizeof" :pointer obj :uint))

(defun strerror (errno)
  (foreign-funcall "strerror" :int errno :string))

(define-condition c-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
	     (format stream "~A: ~A" (text condition) (strerror *errno*)))))

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

@export-accessors
(defclass <termios> ()
  ((iflag :initform 0 :initarg :iflag :accessor iflag)
   (oflag :initform 0 :initarg :oflag :accessor oflag)
   (cflag :initform 0 :initarg :cflag :accessor cflag)
   (lflag :initform 0 :initarg :lflag :accessor lflag)
   (cc :initform nil :initarg :cc :accessor cc)))

(defun make-termios (iflag oflag cflag lflag cc)
  (loop for i from 0 below +NCCS+
     with cc-array = (make-array +NCCS+ :element-type '(mod 32)) 
     do (setf (aref cc-array i) (mem-aref cc 'cc_t i))
     finally (return 
	       (make-instance '<termios> :iflag iflag :oflag oflag :cflag cflag :lflag lflag :cc cc-array))))


(defmacro with-errno-checking (&optional opts ffcall)
  (destructuring-bind (&optional (test '>) (comp -1))
      opts
    `(let ((res ,ffcall))
       (unless (,test res ,comp)
	 (error 'c-error :text (format nil "Error in ffcall")))
       res)))

;;;
;;; Exported posix funs
;;;

@export
(defun close (fd)
  "The close() function shall deallocate the file descriptor indicated by fildes."
  (with-errno-checking ()
      (foreign-funcall "close" :int fd :int)))

@export
(defun open (filename flags &optional mode)
  "The open function creates and returns a new file descriptor for the file named by filename. Initially, the file position indicator for the file is at the beginning of the file. The argument mode is used only when a file is created, but it doesn't hurt to supply the argument in any case."
  (with-foreign-string (fn filename)
    (with-errno-checking ()
	(foreign-funcall "open" :string fn :int flags :int (or mode 0) :int))))

@export
(defun write (fd buf count)
  (unless (>= (array-dimension buf 0) count) (error "Number of bytes to be written exceeds buffer"))
  (with-foreign-object (fbuf :uchar count)
    (loop for i below count
       do (setf (cffi:mem-aref fbuf :uchar i) (aref buf i)))
       (with-errno-checking ()
	 (foreign-funcall "write" :int fd :pointer fbuf :int count :int))))

(defctype size-t :uint)
(defctype ssize-t size-t)

@export
(defun read (fd count)
  (with-foreign-object (fbuf :uchar count)
    (with-errno-checking ()
      (foreign-funcall "read" :int fd :pointer fbuf size-t count ssize-t))
    (loop for i below count
       with larray = (make-array count :element-type '(mod 32))
       do (setf (aref larray i) (mem-aref fbuf :uchar i))
       finally (return larray))))

@export
(defun tcgetattr (fd)
  (with-foreign-object (ptr 'ftermios)
    (with-errno-checking ()
	(foreign-funcall "tcgetattr" :int fd ftermios ptr :int))
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
      (loop with el-array = (cc termios)
	 for i below (min +NCCS+ (array-dimension el-array 0))
	 do (setf (mem-aref c_cc 'cc_t i) (aref el-array i))))
    (with-errno-checking ()
      (foreign-funcall "tcsetattr" :int fd :int (or actions 0) ftermios ptr :int))))

;; TODO: Is there a better way of dealing with c varargs??
@export
(defmacro fcntl (fd cmd &rest args)
  `(with-errno-checking ()
     (foreign-funcall "fcntl"
		      :int ,fd :int ,cmd ,@args :int)))

@export
(defun tcflush (fd queue-selector)
  (with-errno-checking ()
    (foreign-funcall "tcflush" :int fd :int queue-selector :int)))

;;; TODO: Not needed for now but it would probably be nice to have getters as well
(defctype speed-t :uint)

@export
(defun cfsetispeed (termios speed)
  (with-foreign-object (ptr 'ftermios)
    (setf (foreign-slot-value ptr 'ftermios 'c_iflag) (iflag termios)
	  (foreign-slot-value ptr 'ftermios 'c_oflag) (oflag termios)
	  (foreign-slot-value ptr 'ftermios 'c_cflag) (cflag termios)
	  (foreign-slot-value ptr 'ftermios 'c_lflag) (lflag termios))
    (with-foreign-slots ((c_cc) ptr ftermios)
      (loop with el-array = (cc termios)
	 for i below (min +NCCS+ (array-dimension el-array 0))
	 do (setf (mem-aref c_cc 'cc_t i) (aref el-array i))))
    (with-errno-checking ()
      (foreign-funcall "cfsetispeed" ftermios ptr speed-t speed :int))))

@export
(defun cfsetospeed (termios speed)
  (with-foreign-object (ptr 'ftermios)
    (setf (foreign-slot-value ptr 'ftermios 'c_iflag) (iflag termios)
	  (foreign-slot-value ptr 'ftermios 'c_oflag) (oflag termios)
	  (foreign-slot-value ptr 'ftermios 'c_cflag) (cflag termios)
	  (foreign-slot-value ptr 'ftermios 'c_lflag) (lflag termios))
    (with-foreign-slots ((c_cc) ptr ftermios)
      (loop with el-array = (cc termios)
	 for i below (min +NCCS+ (array-dimension el-array 0))
	 do (setf (mem-aref c_cc 'cc_t i) (aref el-array i))))
    (with-errno-checking ()
      (foreign-funcall "cfsetospeed" ftermios ptr speed-t speed :int))))

;;;
;;; select and co.
;;;


(defctype fd-mask :ulong)

(defmacro make-foreign-fd-set-struct ()
  `(cffi:defcstruct fd-set
     (fds-bits fd-mask :count ,(/ +FD-SETSIZE+ +NFDBITS+))))

(make-foreign-fd-set-struct)

(defcstruct timeval
  (tv-sec :long)
  (tv-usec :long))

@export
(defun select (nfds readfds writefds exceptfds timeout)
  (foreign-funcall "select" 
		   :int nfds
		   (:pointer fd-set) readfds
		   (:pointer fd-set) writefds
		   (:pointer fd-set) exceptfds
		   (:pointer timeval) timeout))
