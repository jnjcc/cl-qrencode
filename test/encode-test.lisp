;;;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; "01234567" version 1, level-m, :numeric

(in-package #:cl-qrencode-test)

(defvar text "01234567")
(defvar input (string->input text :mode :numeric))
(defvar data-encoding
  '(0 0 0 1 0 0 0 0   0 0 1 0 0 0 0 0   0 0 0 0 1 1 0 0   0 1 0 1 0 1 1 0
    0 1 1 0 0 0 0 1   1 0 0 0 0 0 0 0   1 1 1 0 1 1 0 0   0 0 0 1 0 0 0 1
    1 1 1 0 1 1 0 0   0 0 0 1 0 0 0 1   1 1 1 0 1 1 0 0   0 0 0 1 0 0 0 1
    1 1 1 0 1 1 0 0   0 0 0 1 0 0 0 1   1 1 1 0 1 1 0 0   0 0 0 1 0 0 0 1))
(defvar ec-coding
  '(1 0 1 0 0 1 0 1   0 0 1 0 0 1 0 0   1 1 0 1 0 1 0 0   1 1 0 0 0 0 0 1
    1 1 1 0 1 1 0 1   0 0 1 1 0 1 1 0   1 1 0 0 0 1 1 1   1 0 0 0 0 1 1 1
    0 0 1 0 1 1 0 0   0 1 0 1 0 1 0 1))

(define-test validate-data-encoding0-7
  (assert-equal data-encoding (cl-qrencode::bstream input)))

(define-test validate-ec-coding0-7
  (assert-equal ec-coding
                (reduce #'append
                        (mapcar #'(lambda (dec)
                                    (cl-qrencode::decimal->bstream dec 8))
                                (car (cl-qrencode::ecc-blocks input))))))

#|
(define-test validate-symbol0-7
  (cl-qrencode::symbol->png (cl-qrencode::input->symbol input)
                            "figure-i1.png" 9 8)
  (format t "compare figure-i1.png with Figure I.1 yourself..."))
|#

(define-test validate-mask0-7
  (multiple-value-bind (masked mask-ref)
      (cl-qrencode::data-masking input)
    (declare (ignore masked))
    (assert-equal '(0 1 0) mask-ref)))