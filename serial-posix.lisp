(in-package #:cerial)
(annot:enable-annot-syntax)

(defclass <serial-posix> (<serial-base>)
  ()
  (:documentation "Serial port class POSIX implementation. Serial port configuration is done with termios and fcntl. Runs on Linux and many other Un*x like systems."))

@export
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

(defmethod device ((s <serial-posix>) port)
  (declare (ignore s))
  (format nil "/dev/ttyS~A" port))

(defmethod open-serial :around ((s <serial-posix>))
  (unless (port s) (error 'serial-error :text "Port must be configured before it can be used."))
  (when (get-fd s) (error 'serial-error :text "Port is already open"))
  (call-next-method))

(defmethod open-serial ((s <serial-posix>))
  (print (port s))
  (let* ((fd (unistd:open
	      (port s)
	      (logior unistd:ORDWR 
		      unistd:ONOCTTY
		      unistd:ONONBLOCK))))
    (setf (slot-value s 'fd) fd)))

(defmethod close-serial ((s <serial-posix>))
  (let ((fd (get-fd s)))
    (when fd
      (unistd:fcntl fd unistd:FSETFL)
      (unistd:close fd)
      (setf (slot-value s 'fd) nil))))

(defmethod configure-port :around ((s <serial-posix>))
  (unless (get-fd s) (error 'serial-error :text "Port has to be open!"))
  (call-next-method s))

(defun get-exported-symbol (symbol &optional (package :cl-user))
  (multiple-value-bind (s e)
      (find-symbol symbol package)
    (when (and s (eql e :EXTERNAL)) s)))

(defun curry-right (fun &rest args)
	   (lambda (&rest more)
	     (apply fun (append more args))))

(defmethod configure-port ((s <serial-posix>))
  (let ((termios (unistd:tcgetattr (get-fd s)))
	(vtime (and (inter-char-timeout s) (floor (* 10 (inter-char-timeout s))))))
    (macrolet ((set-flag (flag &key (on ()) (off ()))
		 `(setf ,flag 
			(logior ,@(mapcar (curry-right 'find-symbol :unistd) on) 
				(logand ,flag (lognot (logior ,@(mapcar (curry-right 'find-symbol :unistd) off)))))))
	       (maybe-set-flag (flag &key (on ()) (off ()) (err nil) (errtxt nil))
		 (labels ((fn (res sym)
			    (let ((sym (cerial::get-exported-symbol sym :unistd)))
			      (if sym
				  (cons sym res)
				  (and err (error err :text errtxt))))))
		   (let ((on (reduce #'fn on :initial-value nil))
			 (off (reduce #'fn off :initial-value nil)))
		     `(setf ,flag (logior ,@on (logand ,flag (lognot (logior ,@off)))))))))
      ;; setup raw mode/ no echo/ binary
      (set-flag (unistd:cflag termios)
		:on ("CLOCAL" "CREAD"))
      (set-flag (unistd:lflag termios)
		:off ("ICANON" "ECHO" "ECHOE" "ECHOK" "ECHONL" "ISIG" "IEXTEN"))
      
      
      (set-flag (unistd:oflag termios)
		:off ("OPOST"))
      (set-flag (unistd:iflag termios)
		:off ("INLCR" "IGNCR" "ICRNL" "IGNBRK"))

      (maybe-set-flag (unistd:iflag termios)
		     :off ("PARMRK"))

      (maybe-set-flag (unistd:iflag termios)
		     :off ("IUCLC"))
      
      ;; setup char length
      (set-flag (unistd:cflag termios)
		:off ("CSIZE"))

      (case (bytesize s)
	(8 (set-flag (unistd:cflag termios) :on ("CS8")))
	(7 (set-flag (unistd:cflag termios) :on ("CS7")))
	(6 (set-flag (unistd:cflag termios) :on ("CS6")))
	(5 (set-flag (unistd:cflag termios) :on ("CS5")))
	(t (error 'value-error (format nil "Invalid char len: ~A" (bytesize s)))))

      ;; setup stopbits
      
      (case (stopbits s)
	(1 (set-flag (unistd:cflag termios) :off (unistd:CSTOPB)))
	((or 2 1.5) (set-flag (unistd:cflag termios) :on (unistd:CSTOPB)))
	(t (error 'value-error :text (format nil "Invalid stop bit spec: ~A" (stopbits s)))))

      ;; setup parity
   
      (set-flag (termios iflag)
		:off (unistd:INPCK unistd:ISTRIP))
      (case (parity s)
	(:PARITY-NONE (set-flag (termios cflag) :off (unistd:PARENB unistd:PARODD)))
	(:PARITY-EVEN (set-flag (termios cflag) :off (unistd:PARODD) :on (unistd:PARENB)))
	(:PARITY-ODD (set-flag (termios cflag) :on (unistd:PARENB unistd:PARODD)))
	(t (error 'value-error :text (format nil "Invalid parity: ~A" (parity s)))))

      (if (get-exported-symbol "IXANY" :unistd)
	  (if (xonxoff s)
	      (set-flag (termios iflag)
			:on (unistd:IXON unistd:IXOFF))
	      (set-flag (termios iflag)
			:off (unistd:IXON unistd:IXOFF unistd:IXANY)))
	  (if (xonxoff s)
	      (set-flag (termios iflag)
			:on (unistd:IXON unistd:IXOFF))
	      (set-flag (termios iflag)
			:off (unistd:IXON unistd:IXOFF))))
      
      (if (get-exported-symbol "CRTSCTS" :unistd)
	  (if (rtscts s)
	      (set-flag (termios iflag)
			:on (unistd:CRTSCTS))
	      (set-flag (termios iflag)
			:off (unistd:CRTSCTS)))
	  (when (get-exported-symbol "CNEW-RTSCTS" :unistd)
	    (if (rtscts s)
		(set-flag (termios iflag)
			  :on (unistd:CNEW_RTSCTS))
		(set-flag (termios iflag)
			  :off (unistd:CNEW_RTSCTS)))))

      (if (or (<= vtime 255) (>= vtime 0))
	  (setf 
	   (aref (termios-cc termios) VMIN) 1
	   (aref (termios-cc termios) VTIME) vtime)
	  (error 'value-error :text (format nil "Invalid parity: ~A" (parity s))))
      
            
      (let* ((ispeed (get-exported-symbol (format nil "B~A" (baudrate s))))
	     (ospeed ispeed))
	(unless ispeed (set-custom-baud-rate (baudrate s))))
      
      (unistd:tcsetattr (fd s) unistd:TCSANOW termios))	  
    
  
	 


