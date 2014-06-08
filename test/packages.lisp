;;;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-user)

(defpackage #:cl-qrencode-test
  (:nicknames :qrcode-test)
  (:use #:cl #:lisp-unit #:cl-qrencode)
  (:import-from #:cl-qrencode
                #:ec-coding)
  (:export #:run-all-tests
           #:function-pattern-symbol
           #:function-pattern-png
           #:raw-symbol
           #:raw-symbol-png))
