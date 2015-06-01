;;;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.
;;;;

(asdf:defsystem #:cl-qrencode
  :description "QR code 2005 encoder in Common Lisp"
  :version "0.1.0"
  :author "jnjcc at live.com"
  :licence "GPL"
  :serial t
  :depends-on (#:zpng)
  :components ((:file "packages")
               (:module "utils"
                        :components ((:file "util")))
               (:module "rs-ecc"
                        :components ((:file "galois")
                                     (:file "bch-ecc")
                                     (:file "rs-ecc")))
               (:module "qrencode"
                        :components ((:file "modes")
                                     (:file "qrspec")
                                     (:file "input")
                                     (:file "bstream")
                                     (:file "codeword")
                                     (:file "matrix")
                                     (:file "mask")
                                     (:file "encode")))
               (:module "image"
                        :components ((:file "png")))))

(asdf:defsystem #:cl-qrencode-test
  :description "QR code encoder test suites"
  :version "0.1.0"
  :author "jnjcc at live.com"
  :licence "GPL"
  :depends-on (#:cl-qrencode)
  :components ((:module "test"
                        :serial t
                        :components ((:file "lisp-unit")
                                     (:file "packages")
                                     (:file "utils")
                                     (:file "pre-spec")
                                     (:file "ecc-test")
                                     (:file "spec-test")
                                     (:file "encode-test")
                                     (:file "tests")))))
