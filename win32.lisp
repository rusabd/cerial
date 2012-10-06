(in-package #:cerial)

(defconstant +GENERIC_READ+  #x80000000)
(defconstant +GENERIC_WRITE+ #x40000000)
(defconstant +OPEN_EXISTING+ 3)


(cffi:load-foreign-library "kernel32")


(cffi:defbitfield dcb-flags
  fBinary;     /* Binary Mode (skip EOF check)    */
  fParity;     /* Enable parity checking          */
  fOutxCtsFlow; /* CTS handshaking on output       */
  fOutxDsrFlow; /* DSR handshaking on output       */
  fDtrControl1;  /* DTR Flow control                */
  fDtrControl2;  /* DTR Flow control                */
  fDsrSensitivity; /* DSR Sensitivity              */
  fTXContinueOnXoff; /* Continue TX when Xoff sent */
  fOutX;       /* Enable output X-ON/X-OFF        */
  fInX;        /* Enable input X-ON/X-OFF         */
  fErrorChar;  /* Enable Err Replacement          */
  fNull;       /* Enable Null stripping           */
  fRtsControl1;  /* Rts Flow control                */
  fRtsControl2;  /* Rts Flow control                */
  fAbortOnError; /* Abort all reads and writes on Error */
)
  

(cffi:defctype dword :uint32)
(cffi:defctype word :uint16)
(cffi:defctype bool :uchar)

(cffi:defcstruct dcb 
  (DCBlength dword);      /* sizeof(DCB)                     */
  (BaudRate dword);       /* Baudrate at which running       */
  (dcbflags dword);
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

(cffi:defcfun (win32-create-file "CreateFileA" :convention :stdcall) :pointer 
  (filename :string)  
  (desired-access :uint32)  
  (share-mode :uint32) 
  (security-attribute :pointer)
  (creation-disposition :uint32)
  (flags-and-attributes :uint32) 
  (template-file :pointer))

(cffi:defcfun (win32-set-comm-state "SetCommState" :convention :stdcall) bool
  (file :pointer)
  (dcb (:pointer dcb)))

(cffi:defcfun (win32-memset "memset") :pointer
  (dest :pointer)
  (fill :int)
  (size :uint))
  
(cffi:defcfun (win32-close-handle "CloseHandle" :convention :stdcall) bool
  (object :pointer))