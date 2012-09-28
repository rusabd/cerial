;;;; cerial.lisp

(in-package #:cerial)

;;; "cerial" goes here. Hacks and glory await!

;;; default values, may be overridden 
(defconstant +BAUDRATES+ '(50 75 110 134 150 200 300 600 1200 1800 2400 4800 9600 19200 38400 57600 115200 230400 460800 500000 576000 921600 1000000 1152000 1500000 2000000 2500000 3000000 3500000 4000000))
(defconstant +BYTESIZES+ '(5 6 7 8))
(defconstant +PARITIES+ '(#\N #\E #\O #\M #\S))
(defconstant +STOPBITS+ '(1 1.5 2))


(define-condition value-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
	     (format stream (text condition)))))

(define-condition serial-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
	     (format stream (text condition)))))
 
(defclass <serial-base> ()
  ((fd :initform nil :reader get-fd)
   (port :initform nil
	 :accessor port
	 :initarg :port
	 :documentation "Number of device starting at zero or a device string")
   
   (baudrate :initform nil
	     :accessor baudrate
	     :initarg :baudrate)
   
   (bytesize :initform nil
	     :accessor bytesize
	     :initarg :bytesize
	     :documentation "Number of databits.")

   (parity :initform nil
	   :accessor parity
	   :initarg :parity
	   :documentation "Parity checking.")

   (stopbits :initform nil
	     :accessor stopbits
	     :initarg :stopbits
	     :documentation "Number of stop-bits")

   (timeout :initform nil
	    :accessor timeout
	    :initarg :timeout
	    :documentation "A timeout value, nil to wait forever")

   (xonxoff :initform nil
	    :accessor xonxoff
	    :initarg :xonxoff
	    :documentation "Software flow control")

   (rtscts :initform nil
	   :accessor rtscts
	   :initarg :rtscts
	   :documentation "RTS/CTS flow control")
   
   (write-timeout :initform nil
		  :accessor write-timeout
		  :initarg :write-timeout
		  :documentation "A timeout value for writes.")
   
   (dsrdtr :initform nil
	   :accessor dsrdtr
	   :initarg :dsrdtr
	   :documentation "None: use rtscts setting, dsrdtr override if True or False")

   (inter-char-timeout :initform nil
		       :accessor inter-char-timeout
		       :initarg :inter-char-timeout
		       :documentation "inter-character timeout, None to disable")

   (supported-parities :initform +PARITIES+
		       :reader get-supported-parities)
   
   (supported-baudrates :initform +BAUDRATES+
			:reader get-supported-baudrates)
   
   (supported-stopbits :initform +STOPBITS+
		       :reader get-supported-stopbits)
   
   (supported-bytesizes :initform +BYTESIZES+
			:reader get-supported-bytesizes)))

;; stubs
(defmethod close-serial ((serial <serial-base>)) (print "CLOSED"))
(defmethod open-serial ((serial <serial-base>)) (setf (slot-value serial 'fd) 1))
(defmethod configure-port ((s <serial-base>)) nil)

(defun maybe-configure-port (s)
  (when (get-fd s)
    (configure-port s)))

(defmethod (setf port) :around (port (s <serial-base>))
  (typecase port
    (string (call-next-method port s))
    (integer (call-next-method (device s port) s))))

(defmethod (setf port) :after (port (s <serial-base>))
  (declare (ignore port))
  (when (get-fd s)
    (close-serial s)
    (open-serial s)))

(defmethod (setf baudrate) :around (baudrate (s <serial-base>))
  (if (find 'baudrate (get-supported-baudrates s))
      (call-next-method)
      (error 'value-error :text (format nil "Not a valid baud rate: ~A" baudrate))))

(defmethod (setf baudrate) :after (baudrate (s <serial-base>))
  (declare (ignore baudrate))
  (maybe-configure-port s))

(defmethod (setf bytesize) :around (size (s <serial-base>))
  (if (find 'size (get-supported-bytesizes s))
      (call-next-method)
      (error 'value-error :text (format nil "Not a valid byte size: ~A" size))))

(defmethod (setf bytesize) :after (size (s <serial-base>))
  (declare (ignore size))
  (maybe-configure-port s))

(defmethod (setf parity) :around (parity (s <serial-base>))
  (if (find 'parity (get-supported-parities s) :test 'char=)
      (call-next-method)
      (error 'value-error :text (format nil "Not a valid parity: ~A" parity))))

(defmethod (setf parity) :after (size (s <serial-base>))
  (maybe-configure-port s))

(defmethod (setf stopbits) :around (bits (s <serial-base>))
  (if (find 'bits (get-supported-stopbits s))
      (call-next-method)
      (error 'value-error :text (format nil "Not a valid stop bit size: ~A" bits))))

(defmethod (setf stopbits) :after (bits (s <serial-base>))
  (declare (ignore bits))
  (maybe-configure-port s))

(defmethod (setf xonxoff) :after (xonxoff (s <serial-base>))
  (declare (ignore xonxoff))
  (maybe-configure-port s))

(defmethod (setf rtscts) :after (rtscts (s <serial-base>))
  (declare (ignore rtscts))
  (maybe-configure-port s))

(defmethod (setf dsrdtr) :after (dsrdtr (s <serial-base>))
  (declare (ignore dsrdtr))
  (maybe-configure-port s))

(defmethod (setf timeout) :around (timeout (s <serial-base>))
  (unless (or (typep 'timeout 'number) (>= timeout 0))
    (error 'value-error :text (format nil "Not a valid timeout: ~A" timeout)))
  (call-next-method))

(defmethod (setf timeout) :after (timeout (s <serial-base>))
  (declare (ignore timeout))
  (maybe-configure-port s))

(defmethod (setf write-timeout) :around (timeout (s <serial-base>))
  (unless (or (typep 'timeout 'number) (>= timeout 0))
    (error 'value-error :text (format nil "Not a valid timeout: ~A" timeout)))
  (call-next-method))

(defmethod (setf write-timeout) :after (timeout (s <serial-base>))
  (declare (ignore timeout))
  (maybe-configure-port s))

(defmethod (setf inter-char-timeout) :around (timeout (s <serial-base>))
  (unless (or (typep 'timeout 'number) (>= timeout 0))
    (error 'value-error :text (format nil "Not a valid timeout: ~A" timeout)))
  (call-next-method))

(defmethod (setf inter-char-timeout) :after (timeout (s <serial-base>))
  (declare (ignore timeout))
  (maybe-configure-port s))


(defmethod print-object ((s <serial-base>) stream)
  (format stream "~A[open=~A](port=~A, baudrate=~A, bytesize=~A, parity=~A, stopbits=~A, timeout=~A, xonxoff=~A, rtscts=~A, dsrdtr=~A)"
	  (type-of s)
	  (when (get-fd s) T)
	  (port s)
	  (baudrate s)
	  (bytesize s)
	  (parity s)
	  (stopbits s)
	  (timeout s)
	  (xonxoff s)
	  (rtscts s)
	  (dsrdtr s)))

(defmethod initialize-instance :after ((s <serial-base>) &key)
  (when (port s)
    (setf (slot-value s 'port) (port s))
    (open-serial s)))

