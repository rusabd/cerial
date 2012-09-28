(in-package #:cerial)
(annot:enable-annot-syntax)

(defclass <serial-posix> (<serial-base>)
  ()
  (:documentation "Serial port class POSIX implementation. Serial port configuration is done with termios and fcntl. Runs on Linux and many other Un*x like systems."))

(defmethod device ((s <serial-posix>) port)
  (declare (ignore s))
  (format nil "/dev/ttyS~A" port))

(defun make-serial-posix (&optional port 
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
  (make-instance '<serial-posix>
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

(defmethod open-serial :around ((s <serial-posix>))
  (unless (port s) (error 'serial-error :text "Port must be configured before it can be used."))
  (when (get-fd s) (error 'serial-error :text "Port is already open"))
  (call-next-method))

(defmethod open-serial ((s <serial-posix>))
  (print (port s))
  (let* ((fd (termios:open
	      (port s)
	      (logior termios:ORDWR 
		      termios:ONOCTTY
		      termios:ONONBLOCK))))
    (setf (slot-value s 'fd) fd)))

(defmethod close-serial ((s <serial-posix>))
  (let ((fd (get-fd s)))
    (when fd
      (fcntl fd termios:FSETFL 0)
      (termios:close fd)
      (setf (slot-value s 'fd) nil))))

(defmethod configure-port :around ((s <serial-posix>))
  (unless (get-fd s) (error 'serial-error :text "Port has to be open!"))
  (call-next-method))

(defun get-exported-symbol (symbol &optional (package :cl-user))
  (multiple-value-bind (s e)
      (find-symbol symbol package)
    (when (and s (eql e :EXTERNAL)) s)))

(defmethod configure-port ((s <serial-posix>))
  (let ((termios (posix:tcgetattr (get-fd s)))
	(vtime (and (inter-char-timeout s) (floor (* 10 (inter-char-timeout s))))))
    (macrolet ((set-flag (flag &key (on ()) (off ()))
		 `(setf ,flag (logior ,@on (logand ,flag (lognot (logior ,@off)))))))
      ;; setup raw mode/ no echo/ binary
      (set-flag (posix:cflag termios)
		:on (posix:CLOCAL posix:CREAD))
      (set-flag (posix:lflag termios)
		:off (posix:ICANON posix:ECHO posix:ECHOE posix:ECHOK posix:ECHONL posix:ISIG posix:IEXTEN))
      
      (set-flag (posix:oflag termios)
		:off (posix:OPOST))
      (set-flag (posix:iflag termios)
		:off (posix:INLCR posix:IGNCR posix:ICRNL posix:IGNBRK))
      (when (get-exported-symbol "PARMRK" :posix)
	(set-flag (posix:iflag termios)
		  :off (posix:PARMRK))))))

      (when (get-exported-symbol "IUCLC" :posix)
	(set-flag (termios-iflag termios)
		  :off (posix:IUCLC)))
      
      ;; setup char length
      (let ((char-length (or (get-exported-symbol (format nil "CS~A" (bytesize s)))
			     (error 'value-error :text (format nil "Invalid char len: ~A" (bytesize s))))))
	(set-flat (termios-cflag termios)
		  :on (char-length)
		  :off (posix:CSIZE)))
      ;; setup stopbits
      
      (case (stopbits s)
	(1 (set-flag (termios clfag)
		     :off (posix:CSTOPB)))
	((or 2 1.5) (set-flag (termios clfag)
			      :on (posix:CSTOPB)))
	(t (error 'value-error :text (format nil "Invalid stop bit spec: ~A" (stopbits s)))))

      ;; setup parity
   
      (set-flag (termios iflag)
		:off (posix:INPCK posix:ISTRIP))
      (case (parity s)
	(:PARITY-NONE (set-flag (termios cflag) :off (posix:PARENB posix:PARODD)))
	(:PARITY-EVEN (set-flag (termios cflag) :off (posix:PARODD) :on (posix:PARENB)))
	(:PARITY-ODD (set-flag (termios cflag) :on (posix:PARENB posix:PARODD)))
	(t (error 'value-error :text (format nil "Invalid parity: ~A" (parity s)))))

      (if (get-exported-symbol "IXANY" :posix)
	  (if (xonxoff s)
	      (set-flag (termios iflag)
			:on (posix:IXON posix:IXOFF))
	      (set-flag (termios iflag)
			:off (posix:IXON posix:IXOFF posix:IXANY)))
	  (if (xonxoff s)
	      (set-flag (termios iflag)
			:on (posix:IXON posix:IXOFF))
	      (set-flag (termios iflag)
			:off (posix:IXON posix:IXOFF))))
      
      (if (get-exported-symbol "CRTSCTS" :posix)
	  (if (rtscts s)
	      (set-flag (termios iflag)
			:on (posix:CRTSCTS))
	      (set-flag (termios iflag)
			:off (posix:CRTSCTS)))
	  (when (get-exported-symbol "CNEW-RTSCTS" :posix)
	    (if (rtscts s)
		(set-flag (termios iflag)
			  :on (posix:CNEW_RTSCTS))
		(set-flag (termios iflag)
			  :off (posix:CNEW_RTSCTS)))))

      (if (or (<= vtime 255) (>= vtime 0))
	  (setf 
	   (aref (termios-cc termios) VMIN) 1
	   (aref (termios-cc termios) VTIME) vtime)
	  (error 'value-error :text (format nil "Invalid parity: ~A" (parity s))))
      
            
      (let* ((ispeed (get-exported-symbol (format nil "B~A" (baudrate s))))
	     (ospeed ispeed))
	(unless ispeed (set-custom-baud-rate (baudrate s))))
      
      (posix:tcsetattr (fd s) posix:TCSANOW termios)))
	
      
	
	  

	      
	  
	  ;; setup baud-rate
    
  
	 


