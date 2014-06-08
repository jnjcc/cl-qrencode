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
           #:encode-symbol-bytes
           #:qr-symbol
           #:dark-module-p
           #:encode-png
           #:encode-png-bytes
           #:read-file-content
           #:sdebug
           #:undebug))
