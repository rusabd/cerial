(in-package #:cerial)

(defclass <serial-win32> (<serial-base>)
  ((handler :reader handler))
  (:documentation "Serial port class WIN32 implementation."))

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
                           (inter-char-timeout nil))
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
		 :inter-char-timeout inter-char-timeout))

(defmethod device ((s <serial-win32>) port)
  (declare (ignore s))
  (format nil "\\\\.\\~A" port))


(defmethod open-serial ((s <serial-win32>))
  (let* ((null (cffi:null-pointer))
	 (handler (win32-create-file (device s (port s)) (logxor +GENERIC_READ+ +GENERIC_WRITE+) 0 null +OPEN_EXISTING+ 0 null)))
    ;; FIXME - add check for handler!

    (cffi:with-foreign-object (ptr 'dcb)
      (win32-memset ptr 0 (cffi:foreign-type-size 'dcb))
      (cffi:with-foreign-slots ((baudrate bytesize parity stopbits dcbflags) ptr dcb)
	  (setf baudrate 9600)
	(setf bytesize 8)
	(setf stopbits 0)
	(setf parity 0)
	(setf dcbflags (cffi:foreign-bitfield-value 'dcb-flags '(fbinary))))
      (win32-set-comm-state handler ptr))
    (setf (slot-value s 'handler) handler)))

(defmethod close-serial ((s <serial-win32>))
  (with-slots (handler) s
    (win32-close-handle handler)
    (cffi:foreign-free handler)))

(defmethod set-custom-baud-rate ((s <serial-win32>))
  (error 'value-error :text (format nil "Invalid baudrate: ~A" (baudrate s))))

(defmethod configure-port ((s <serial-win32>))
  (error "not implemented"))

(defmethod write-serial-byte-seq ((s <serial-win32>) byte-seq)
  (let ((seq-size (length byte-seq)))
    (cffi:with-foreign-object (buffer :char seq-size)
      (cffi:with-foreign-object (writtenbytes 'word)	
	(with-slots (handler) s
	  (dotimes (idx seq-size)
	    (setf (cffi:mem-aref buffer :char idx) (aref byte-seq idx)))
	  (win32-confirm (win32-write-file handler buffer seq-size writtenbytes (cffi:null-pointer))
			 (cffi:mem-ref writtenbytes 'word)
			 (error "could not write to device")))))))

(defmethod read-serial-byte-seq ((s <serial-win32>) count)
  (cffi:with-foreign-object (buffer :char count)
    (cffi:with-foreign-object (readbytes 'word)
      (with-slots (handler) s
	  (win32-confirm (win32-read-file handler buffer count readbytes (cffi:null-pointer))
			 (loop with size = (cffi:mem-ref readbytes 'word)
			    with result = (make-array size :element-type '(integer 0 255))
			    for idx below size
			    do (setf (aref result idx) (cffi:mem-aref buffer :char  idx))
			    finally (return result))
			 (error "could not read from device"))))))
  
;; TODO: Blocking versions using select
