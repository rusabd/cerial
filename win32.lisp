(in-package #:cerial)

(defconstant +GENERIC_READ+  #x80000000)
(defconstant +GENERIC_WRITE+ #x40000000)
(defconstant +FILE_ATTRIBUTE_NORMAL+ #x80)
(defconstant +FILE_FLAG_OVERLAPPED+ #x40000000)
(defconstant +OPEN_EXISTING+ 3)

(defconstant +MAXDWORD+ 4294967295)

(defconstant +ONESTOPBIT+    0)
(defconstant +ONE5STOPBITS+  1)
(defconstant +TWOSTOPBITS+   2)

(defconstant +CBR_110+             110)
(defconstant +CBR_300+             300)
(defconstant +CBR_600+             600)
(defconstant +CBR_1200+            1200)
(defconstant +CBR_2400+            2400)
(defconstant +CBR_4800+            4800)
(defconstant +CBR_9600+            9600)
(defconstant +CBR_14400+           14400)
(defconstant +CBR_19200+           19200)
(defconstant +CBR_38400+           38400)
(defconstant +CBR_56000+           56000)
(defconstant +CBR_57600+           57600)
(defconstant +CBR_115200+          115200)
(defconstant +CBR_128000+          128000)
(defconstant +CBR_256000+          256000)

(defconstant +XON+ 17)
(defconstant +XOFF+ 19)

(defconstant +NOPARITY+            0)
(defconstant +ODDPARITY+           1)
(defconstant +EVENPARITY+          2)
(defconstant +MARKPARITY+          3)
(defconstant +SPACEPARITY+         4)

(defconstant +RTS_CONTROL_DISABLE+   0)
(defconstant +RTS_CONTROL_ENABLE+    1)
(defconstant +RTS_CONTROL_HANDSHAKE+ 2)
(defconstant +RTS_CONTROL_TOGGLE+    3)
(defconstant +SETRTS+                3)
(defconstant +CLRRTS+                4)

(defconstant +DTR_CONTROL_DISABLE+   0)
(defconstant +DTR_CONTROL_ENABLE+    1)
(defconstant +DTR_CONTROL_HANDSHAKE+ 2)
(defconstant +SETDTR+                5)
(defconstant +CLRDTR+                6)

(defconstant +INFINITE+ #xFFFFFFFF)

(defconstant +PURGE_TXCLEAR+ 4)
(defconstant +PURGE_TXABORT+ 1)
(defconstant +PURGE_RXCLEAR+ 8)
(defconstant +PURGE_RXABORT+ 2)

(cffi:load-foreign-library "kernel32")
  
(cffi:defctype dword :uint32)
(cffi:defctype word :uint16)
(cffi:defctype bool :uchar)

(cffi:defcstruct dcb 
  (DCBlength dword);      /* sizeof(DCB)                     */
  (BaudRate dword);       /* Baudrate at which running       */
  (fBinary dword);     /* Binary Mode (skip EOF check)    */
  (fParity dword);     /* Enable parity checking          */
  (fOutxCtsFlow dword); /* CTS handshaking on output       */
  (fOutxDsrFlow dword); /* DSR handshaking on output       */
  (fDtrControl dword);  /* DTR Flow control                */
  (fDsrSensitivity dword); /* DSR Sensitivity              */
  (fTXContinueOnXoff dword); /* Continue TX when Xoff sent */
  (fOutX dword);       /* Enable output X-ON/X-OFF        */
  (fInX dword);        /* Enable input X-ON/X-OFF         */
  (fErrorChar dword);  /* Enable Err Replacement          */
  (fNull dword);       /* Enable Null stripping           */
  (fRtsControl dword);  /* Rts Flow control                */
  (fAbortOnError dword); /* Abort all reads and writes on Error */
  (wReserved word);       /* Not currently used              */
  (XonLim word);          /* Transmit X-ON threshold         */
  (XoffLim word);         /* Transmit X-OFF threshold        */
  (ByteSize :uint8);        /* Number of bits/byte, 4-8        */
  (Parity :uint8);          /* 0-4=None,Odd,Even,Mark,Space    */
  (StopBits :uint8);        /* 0,1,2 = 1, 1.5, 2               */
  (XonChar :char);         /* Tx and Rx X-ON character        */
  (XoffChar :char);        /* Tx and Rx X-OFF character       */
  (ErrorChar :char);       /* Error replacement char          */
  (EofChar :char);         /* End of Input character          */
  (EvtChar :char);         /* Received Event character        */
  (wReserved1 word));      /* Fill for now.                   */

(cffi:defcstruct commtimeouts
  (ReadIntervalTimeout dword)
  (ReadTotalTimeoutMultiplier dword)
  (ReadTotalTimeoutConstant dword)
  (WriteTotalTimeoutMultiplier dword)
  (WriteTotalTimeoutConstant dword))

(cffi:defcstruct comstat
  (fCtsHold dword)
  (fDsrHold  dword)
  (fRlsdHold dword)
  (fXoffHold dword)
  (fXoffSent dword)
  (fEof dword)
  (fTxim dword)
  (fReserved dword)
  (cbInQue dword)
  (cbOutQue dword))

(cffi:defctype pvoid (:pointer :void)) 
(cffi:defctype lpvoid (:pointer :void)) 
(cffi:defctype dword-ptr (:pointer dword))
(cffi:defctype ulong-ptr dword-ptr)
(cffi:defctype handle pvoid)
(cffi:defctype lpdword (:pointer dword))
(cffi:defctype lpword (:pointer word))
(cffi:defctype lpcomstat (:pointer comstat))
(cffi:defctype lpctstr :string)

(cffi:defcstruct overlapped-us
  (Offset dword)
  (OffsetHigh dword))

(cffi:defcunion overlapped-u
  (overlapped-us overlapped-us)
  (Pointer pvoid))

(cffi:defcstruct overlapped
  (Internal ulong-ptr)
  (InternalHigh ulong-ptr)
  (overlapped-u overlapped-u)
  (hEvent handle))

(cffi:defctype lpoverlapped (:pointer overlapped))

(cffi:defcstruct security-attributes
  (nLength dword)
  (lpSecurityDescriptor lpvoid)
  (bInheritHandle bool))

(cffi:defctype lpsecurity-attributes (:pointer security-attributes))


(cffi:defcfun (win32-reset-event "ResetEvent" :convention :stdcall) bool
  (hevent handle))

(cffi:defcfun (win32-clear-comm-error "ClearCommError" :convention :stdcall) bool
  (hfile handle)
  (lperrors lpdword)
  (lpstat lpcomstat))

(cffi:defcfun (win32-wait-for-single-object "WaitForSingleObject" :convention :stdcall) bool
  (hHandle handle)
  (dwMilliseconds dword))

(cffi:defcfun (win32-get-overlapped-result "GetOverlappedResult" :convention :stdcall) bool
  (hFile handle)
  (lpOverlapped lpoverlapped)
  (lpNumberOfBytesTransferred lpword)
  (bWait bool))
  
(cffi:defcfun (win32-purge-comm "PurgeComm" :convention :stdcall) bool
  (hFile handle)
  (flags dword))

(cffi:defcfun (win32-create-event "CreateEvent" :convention :stdcall) bool
  (lpEventAttributes lpsecurity-attributes)
  (bManualReset bool)
  (bInitialState bool)
  (lpName lpctstr))

(cffi:defcfun (win32-get-comm-timeouts "GetCommTimeouts" :convention :stdcall) bool
  (hFile handle)
  (timeouts (:pointer commtimeouts)))

(cffi:defcfun (win32-create-file "CreateFileA" :convention :stdcall) :pointer 
  (filename :string)  
  (desired-access :uint32)  
  (share-mode :uint32) 
  (security-attribute :pointer)
  (creation-disposition :uint32)
  (flags-and-attributes :uint32) 
  (template-file :pointer))

(cffi:defcfun (win32-setup-comm "SetupComm" :convention :stdcall) bool
  (file :pointer)
  (dwInQueue dword)
  (dwOutQueue dword))

(cffi:defcfun (win32-escape-comm-function "EscapeCommFunction" :convention :stdcall) bool
  (file :pointer)
  (escape dword))

(cffi:defcfun (win32-set-comm-timeouts "SetCommTimeouts" :convention :stdcall) bool
  (file :pointer)
  (timeouts (:pointer commtimeouts)))

(cffi:defcfun (win32-set-comm-state "SetCommState" :convention :stdcall) bool
  (file :pointer)
  (dcb (:pointer dcb)))

(cffi:defcfun (win32-get-comm-state "GetCommState" :convention :stdcall) bool
  (file :pointer)
  (dcb (:pointer dcb)))

(cffi:defcfun (win32-memset "memset") :pointer
  (dest :pointer)
  (fill :int)
  (size :uint))
  
(cffi:defcfun (win32-close-handle "CloseHandle" :convention :stdcall) bool
  (object :pointer))

(cffi:defcfun (win32-read-file "ReadFile" :convention :stdcall) bool
  (file :pointer)
  (buffer :pointer)
  (size word)
  (readBytes (:pointer word))
  (overlapped :pointer))

(cffi:defcfun (win32-write-file "WriteFile" :convention :stdcall) bool
  (file :pointer)
  (buffer :pointer)
  (size word)
  (writtenBytes (:pointer word))
  (overlapped-p :pointer))

(defun valid-pointer-p (pointer)
  (not (cffi:pointer-eq pointer (cffi:make-pointer #xFFFFFFFF))))

(defmacro win32-confirm (form success fail)
  `(if (zerop ,form)
       ,fail
       ,success))

(defmacro win32-onerror (form &body error-form)
  `(win32-confirm ,form
		  t
		  (progn
		    ,@error-form)))
