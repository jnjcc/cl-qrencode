;;;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.
;;;;

(asdf:defsystem #:cl-qrencode
  :description "QR code 2005 encoder in Common Lisp"
  :version "0.1.1"
  :author "jnjcc at live.com"
  :licence "GPL"
  :serial t
  :depends-on (#:zpng)
  :in-order-to ((asdf:test-op (asdf:load-op #:cl-qrencode-test)))
  :perform (asdf:test-op :after (op c)
             (funcall (find-symbol (symbol-name '#:run-all-tests) '#:cl-qrencode-test)))
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
