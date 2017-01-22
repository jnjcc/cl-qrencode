;;;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.
;;;;

(asdf:defsystem #:cl-qrencode-test
  :description "QR code encoder test suites"
  :version "0.1.1"
  :author "jnjcc at live.com"
  :licence "GPL"
  :depends-on (#:cl-qrencode #:lisp-unit)
  :components ((:module "test"
                        :serial t
                        :components ((:file "packages")
                                     (:file "utils")
                                     (:file "pre-spec")
                                     (:file "ecc-test")
                                     (:file "spec-test")
                                     (:file "encode-test")
                                     (:file "tests")))))
