;;;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-user)

(defpackage #:cl-qrencode
  (:nicknames :qrcode)
  (:use #:cl)
  (:import-from #:zpng
                #:png
                #:data-array
                #:write-png)
  (:export #:encode-symbol
           #:encode-symbol-bytes ; this should have been deprecated
           ;; QR code representation
           ;; this should be enough to write another backend for QR symbol
           #:qr-symbol
           #:matrix
           #:modules
           #:dark-module-p
           ;; png backend
           ;; strictly speaking, this should not be part of this package
           #:encode-png
           #:encode-png-bytes ; this should have been deprecated
           #:encode-png-stream
           ; #:encode-png-bytes-stream
           #:read-file-content
           #:sdebug
           #:undebug))
