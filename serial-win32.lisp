(in-package #:cerial)
(annot:enable-annot-syntax)

@export-accessors
(defclass <serial-win32> (<serial-base>)
  ((buffer-size :initform nil
		:accessor buffer-size
		:initarg :buffer-size
		:documentation "The recommended size of the device's internal input buffer, in bytes")
   (rts-state :initform nil
	      :reader rts-state
	      :initarg :rts-state
	      :documentation "Terminal status line: Request to Send")
   (dtr-state :initform nil
	      :reader dtr-state
	      :initarg :dtr-state
	      :documentation "Terminal status line: Data Terminal Ready")
   (rts-toggle :initform nil
	       :reader rts-toggle
	       :initarg :rts-toggle
	       :documentation "RTS toggle control setting")
   (overlapped-read :initform nil
		    :reader overlapped-read
		    :initarg :overlapped-read)
   (overlapped-write :initform nil
		     :reader overlapped-write
		     :initarg :overlapped-write)
   (org-timeouts :initform nil
		 :initarg :org-timeouts))
   (:documentation "Serial port class WIN32 implementation."))

@export
(defun make-serial-port (&optional port 
                         &key (baudrate 9600)
			   (bytesize 8)
			   (parity :PARITY-NONE)
			   (stopbits 1)
			   (timeout nil)
			   (xonxoff nil)
			   (rtscts nil)
			   (write-timeout nil)
			   (dsrdtr nil)
			   (inter-char-timeout nil)
			   (buffer-size nil))
  (make-instance '<serial-win32>
		 :port port
		 :baudrate baudrate
		 :bytesize bytesize
		 :parity parity
		 :stopbits stopbits
		 :timeout timeout
		 :xonxoff xonxoff
		 :rtscts rtscts
		 :write-timeout write-timeout
		 :dsrdtr dsrdtr
		 :inter-char-timeout inter-char-timeout
		 :buffer-size buffer-size))

(defmethod device ((s <serial-win32>) port)
  (declare (ignore s))
  (format nil "\\\\.\\~A" port))


(defun stopbits->win32 (stopbits)
  (cond
    ((= stopbits 1) +ONESTOPBIT+)
    ((= stopbits 1.5) +ONE5STOPBITS+)
    ((= stopbits 2) +TWOSTOPBITS+)
    (t (error 'serial-error :text "unsupported stopbits"))))

(defun baudrate->win32 (baudrate)
  (cond 
    ((= baudrate 110)             +CBR_110+)
    ((= baudrate 300)             +CBR_300+)
    ((= baudrate 600)             +CBR_600+)
    ((= baudrate 1200)            +CBR_1200+)
    ((= baudrate 2400)            +CBR_2400+)
    ((= baudrate 4800)            +CBR_4800+)
    ((= baudrate 9600)            +CBR_9600+)
    ((= baudrate 14400)           +CBR_14400+)
    ((= baudrate 19200)           +CBR_19200+)
    ((= baudrate 38400)           +CBR_38400+)
    ((= baudrate 56000)           +CBR_56000+)
    ((= baudrate 57600)           +CBR_57600+)
    ((= baudrate 115200)          +CBR_115200+)
    ((= baudrate 128000)          +CBR_128000+)
    ((= baudrate 256000)          +CBR_256000+)
    (t (error 'serial-error :text "unsupported baudrate"))))

(defun parity->win32 (parity)
  (ecase parity
    (:PARITY-NONE +NOPARITY+)
    (:PARITY-EVEN +EVENPARITY+)
    (:PARITY-ODD +ODDPARITY+)
    (:PARITY-MARK +MARKPARITY+)
    (:PARITY-SPACE +SPACEPARITY+)))

(defmethod initialize-instance :before ((s <serial-win32>) &key)
  (setf (slot-value s 'rts-toggle) nil)
  (setf (slot-value s 'rts-state) +RTS_CONTROL_ENABLE+)
  (setf (slot-value s 'rts-state) +DTR_CONTROL_ENABLE+))

@export
(defmethod open-serial ((s <serial-win32>))
  (let* ((null (cffi:null-pointer))
	 (handler (win32-create-file (device s (port s)) (logxor +GENERIC_READ+ +GENERIC_WRITE+) 0 null +OPEN_EXISTING+ (logxor +FILE_ATTRIBUTE_NORMAL+ +FILE_FLAG_OVERLAPPED+) null)))
    (unless (valid-pointer-p handler)
      (error 'serial-error :text "CreateFile failed"))
    (when (buffer-size s) 
      (win32-onerror (win32-setup-comm handler (buffer-size s) (buffer-size s))
	(error 'serial-error :text "SetupComm failed")))
    
    (setf (slot-value s 'fd) handler)))

@export
(defmethod close-serial ((s <serial-win32>))
  (with-slots (fd) s
    (win32-close-handle fd)))

(defmethod set-timeout ((s <serial-win32>))
  "Set Windows timeout values."
  (with-slots (timeout inter-char-timeout write-timeout fd) s
    (flet ((read-timeouts (timeout)
             (cond
               ((not timeout) `(0 0 0 0 0))
               ((zerop timeout) `(,+MAXDWORD+ 0 0 0 0))
               (t `(0 0 ,(ceiling (* timeout 1000)) 0 0))))
           (inter-char-timeouts (timeouts)
             (destructuring-bind (nil &rest x) timeouts
               (if (and (\= timeout 0) inter-char-timeout)
                   (cons (ceiling (* inter-char-timeout 1000)) x))))
           (write-timeouts (timeouts)
             (destructuring-bind ((a b c &rest nil)) timeouts
               (if (zerop write-timeout)
                   `(,a ,b ,c 0 ,+MAXDWORD+)
                   `(,a ,b ,c 0 ,(ceiling (* write-timeout 1000)))))))
      (destructuring-bind (read-interval-timeout 
			   read-total-timeout-multiplier 
			   read-total-timeout-constant 
			   write-total-timeout-multiplier 
			   write-total-timeout-constant)
	  (write-timeouts (inter-char-timeouts (read-timeouts timeout)))
	(cffi:with-foreign-object (ptr 'commtimeouts)
	  (cffi:with-foreign-slots ((ReadIntervalTimeout 
				     ReadTotalTimeoutMultiplier 
				     ReadTotalTimeoutConstant 
				     WriteTotalTimeoutMultiplier 
				     WriteTotalTimeoutConstant) ptr commtimeouts)
	    (setf ReadIntervalTimeout read-interval-timeout
		  ReadTotalTimeoutMultiplier read-total-timeout-multiplier
		  ReadTotalTimeoutConstant read-total-timeout-constant
		  WriteTotalTimeoutMultiplier write-total-timeout-multiplier
		  WriteTotalTimeoutConstant write-total-timeout-constant)
	    (win32-onerror (win32-set-comm-timeouts fd ptr)
	      (error 'serial-error :text "SetCommTimeouts failed"))))))))

(defmethod configure-port :before ((s <serial-win32>))
  (cffi:with-foreign-object (ptr 'commtimeouts)
    (win32-get-comm-timeouts (get-fd s) ptr)
    (setf (slot-value s 'org-timeouts) ptr)))

(defmethod configure-port :after ((s <serial-win32>))
  (with-slots (fd) s
    (win32-purge-comm fd 
		      (logior +PURGE_TXCLEAR+ +PURGE_TXABORT+ 
			     +PURGE_RXCLEAR+ +PURGE_RXABORT+))
    (cffi:with-foreign-object (ptr 'overlapped)
      (cffi:with-foreign-slots ((hEvent) ptr overlapped)
	(setf hEvent (win32-create-event (cffi:null-pointer) 1 0 (cffi:null-pointer)))
	(setf (slot-value s 'overlapped-read) ptr)))
    (cffi:with-foreign-object (ptr 'overlapped)
      (cffi:with-foreign-slots ((hEvent) ptr overlapped)
	(setf hEvent (win32-create-event (cffi:null-pointer) 0 0 (cffi:null-pointer)))
	(setf (slot-value s 'overlapped-write) ptr)))))

(defmethod configure-port ((s <serial-win32>))
  (with-slots (fd xonxoff dsrdtr baudrate bytesize stopbits parity rtscts dtr-state rts-state rts-toggle) s
    (cffi:with-foreign-object (ptr 'dcb)
      (win32-memset ptr 0 (cffi:foreign-type-size 'dcb))
      (cffi:with-foreign-slots ((DCBlength) ptr dcb)
	  (setf DCBlength (cffi:foreign-type-size 'dcb)))
      (win32-onerror (win32-get-comm-state fd ptr)
	(error 'serial-error :text "GetCommState failed"))
      (cffi:with-foreign-slots ((baudrate 
				 bytesize 
				 parity 
				 stopbits 
				 fbinary 
				 fRtsControl 
				 fDtrControl 
				 fOutxCtsFlow
				 fOutxDsrFlow
				 fOutX fInX
				 fNull
				 fErrorChar
				 fAbortOnError
				 XonChar
				 XoffChar) ptr dcb)
	(setf baudrate (baudrate->win32 baudrate)
	      bytesize bytesize
	      stopbits (stopbits->win32 stopbits)
	      parity   (parity->win32 parity)
	      fbinary  1
	      fOutxDsrFlow  dsrdtr
	      fOutX         xonxoff
	      fInX          xonxoff
	      fNull         0
	      fErrorChar    0
	      fAbortOnError 0
	      XonChar       +XON+
	      XoffChar      +XOFF+)
	 (cond 
            (rtscts (setf fRtsControl +RTS_CONTROL_HANDSHAKE+))
            (rts-toggle (setf fRtsControl +RTS_CONTROL_TOGGLE+))
            (t (setf fRtsControl rts-state)))
          (if dsrdtr
              (setf fDtrControl +DTR_CONTROL_HANDSHAKE+)
              (setf fDtrControl dtr-state))
          (if rts-toggle
              (setf fOutxCtsFlow 0)
              (setf fOutxCtsFlow rtscts))
	(win32-onerror (win32-set-comm-state fd ptr)
	  (error 'serial-error :text "SetCommState failed"))))))


@export
(defmethod write-serial-byte ((s <serial-win32>) byte)
  (write-serial-byte-seq s (make-array 1 :initial-element byte)))

@export
(defmethod write-serial-byte-seq ((s <serial-win32>) byte-seq)
  (let ((seq-size (length byte-seq)))
    (cffi:with-foreign-object (buffer :char seq-size)
      (cffi:with-foreign-object (writtenbytes 'word)	
	(with-slots (fd) s
	  (dotimes (idx seq-size)
	    (setf (cffi:mem-aref buffer :char idx) (aref byte-seq idx)))
	  (win32-confirm (win32-write-file fd buffer seq-size writtenbytes (cffi:null-pointer))
			 (cffi:mem-ref writtenbytes 'word)
			 (error 'serial-error :text "could not write to device")))))))

@export
(defmethod (setf rts-state) :around (enabledp (s <serial-win32>))
  (if enabledp
      (call-next-method +RTS_CONTROL_ENABLE+ s)
      (call-next-method +RTS_CONTROL_DISABLE+ s)))

@export
(defmethod (setf rts-state) :after (enabledp (s <serial-win32>))
  (when (openp s)
    (with-slots (fd) s
      (if enabledp
	  (win32-escape-comm-function fd +SETRTS+)
	  (win32-escape-comm-function fd +CLRRTS+)))))

@export
(defmethod (setf dtr-state) :around (enabledp (s <serial-win32>))
  (if enabledp
      (call-next-method +DTR_CONTROL_ENABLE+ s)
      (call-next-method +DTR_CONTROL_DISABLE+ s)))

@export
(defmethod (setf dtr-state) :after (enabledp (s <serial-win32>))
  (when (openp s)
    (with-slots (fd) s
      (if enabledp
	  (win32-escape-comm-function fd +SETDTR+)
	  (win32-escape-comm-function fd +CLRDTR+)))))

@export
(defmethod (setf str-toggle) :after (enabledp (s <serial-win32>))
  (declare (ignore enabledp))
  (when (openp s)
    (configure-port s)))

@export
(defmethod read-serial-byte ((s <serial-win32>))
  (aref (read-serial-byte-seq s 1)))

(defmethod reset-overlapped-read-event ((s <serial-win32>))
  (cffi:with-foreign-slots ((hEvent) (overlapped-read s) overlapped)
    (win32-reset-event hEvent)))

@export
(defmethod read-serial-byte-seq ((s <serial-win32>) count)
  "Read count bytes from the serial port. If a timeout is set it may 
return less bytes than requested. With no timeout it will block
until the requested number of bytes is read."
  (when (> count 0)
    (reset-overlapped-read-event s)
    (with-slots (fd) s	
      (cffi:with-foreign-objects ((comstat-ptr 'comstat)
				  (flags 'dword))				  
	(win32-onerror
	    (win32-clear-comm-error fd flags comstat-ptr)
	  (error 'serial-error :text "ClearCommError failed."))
	(cffi:with-foreign-object (buffer :char count)
	  (cffi:with-foreign-object (readbytes 'word)
	    (cffi:with-foreign-slots ((cbInQue) comstat-ptr comstat)
	      (cffi:with-foreign-slots ((hEvent) (overlapped-read s) overlapped)
		(let ((n (min cbInQue count)))
		  (win32-confirm (win32-read-file fd buffer n readbytes (overlapped-read s))
				 (progn
				   (cond
				     ((and (= (timeout s) 0) (> n 0))
				      (win32-wait-for-single-object hEvent +INFINITE+))
				     ((/= (timeout s))
				      (win32-get-overlapped-result fd (overlapped-read s) readbytes T))
				     (t nil))
				   (loop with size = (cffi:mem-ref readbytes 'word)
				      with result = (make-array size :element-type '(integer 0 255))
				      for idx below size
				      do (setf (aref result idx) (cffi:mem-aref buffer :char  idx))
				      finally (return result)))
				 (error 'serial-error :text "could not read from device")))))))))))
	    
@export
(defmethod print-object :after ((s <serial-base>) stream)
  (with-slots (buffer-size rts-state rts-toggle dtr-toggle) s
    (format stream ", buffer-size: ~A, rts-state: ~A, rts-toggle: ~A, dtr-state: ~A"
	    buffer-size rts-state rts-toggle dtr-toggle)))
