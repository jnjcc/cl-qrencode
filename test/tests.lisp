;;;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-qrencode-test)

(defun run-all-tests ()
  (let ((result (run-tests :all :cl-qrencode-test)))
    (test-names result)
    result))
