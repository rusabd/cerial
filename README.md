# CeriaL
Mmmmh...Crunchy!

## Usage
WARNING! I'm still experimenting...Don't get used to the flavour just yet.

```common-lisp
;; serial port to write to
(setq *sout* (cerial:make-serial "pseudo0"))

;; the other end
(setq *sout* (cerial:make-serial "pseudo1"))

;; write some 
(cerial:write-byte-seq *sout* (flexi-streams:string-to-octets "Hello World!"))

;; read some
(flexi-streams:octets-to-string (cerial:read-byte-seq *sin* 12))

"Hello World!"
```

For a full description of the API see the wiki pages of the future!

