;;;; Copyright (c) 2011-2014 jncc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-qrencode-test)

(define-test validate-gen-poly
  (loop for ec from 2 to 68 do
       (let ((expect (aref *generator-poly* ec)))
         (when expect
           (let* ((rs (make-instance 'cl-qrencode::rs-ecc :ec ec :k nil))
                  (gpoly (cl-qrencode::gen-poly-gflog rs)))
             (assert-equal expect gpoly))))))

(defun encode-alnum (text)
  (let ((input (string->input text :version 1 :level :level-m :mode :alnum)))
    input))

(define-test validate-hello-world-ecc
  "blocks input"
  (let ((input (encode-alnum "HELLO WORLD")))
    (assert-equal '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17)
                  (car (cl-qrencode::blocks input)))
    (assert-equal '(196 35 39 119 235 215 231 226 93 23)
                  (car (cl-qrencode::ecc-blocks input)))))

(define-test validate-version-ecc
  (loop for v from 7 to 40 do
       (let ((vib (cl-qrencode::version-ecc v)))
         (assert-eql (aref *version-info-hex* (- v 7))
                     (cl-qrencode::bstream->decimal vib 18)))))

(define-test validate-format-ecc
  (let ((idx 0))
    ;; 00 01 10 11
    (dolist (l '(:level-m :level-l :level-h :level-q))
      ;; 000 ~ 111
      (dotimes (m cl-qrencode::*mask-pattern-num*)
        (let ((seq (append (cl-qrencode::level-indicator l)
                           (cl-qrencode::mask-pattern-ref m)))
              (fib (cl-qrencode::format-ecc l m)))
          (assert-eql idx (cl-qrencode::bstream->decimal seq 5))
          (assert-eql (aref *format-info-hex* idx)
                      (cl-qrencode::bstream->decimal fib 15))
          (incf idx))))))
