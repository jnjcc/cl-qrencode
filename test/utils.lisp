;;;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; test utils

(in-package #:cl-qrencode-test)

(defun function-pattern-symbol (version)
  "qr symbol only consisted of function patterns"
  (let* ((modules (cl-qrencode::matrix-modules version))
         (matrix (cl-qrencode::make-modules-matrix modules)))
    (cl-qrencode::function-patterns matrix version)
    (make-instance 'cl-qrencode::qr-symbol :matrix matrix :modules modules)))

(defun string->input (text &key (version 1) (level :level-m) (mode :numeric))
  (let ((bytes (cl-qrencode::ascii->bytes text)))
    (cl-qrencode::bytes->input bytes version level mode)))

(defun raw-symbol (text &key (version 1) (level :level-m) (mode nil))
  "raw symbol, without masking"
  (let ((input (string->input text :version version :level level :mode mode)))
    (make-instance 'cl-qrencode::qr-symbol
                   :matrix (cl-qrencode::matrix input)
                   :modules (cl-qrencode::matrix-modules
                             (cl-qrencode::version input)))))

(defun function-pattern-png (version)
  "dump function patterns into png"
  (let ((fp (function-pattern-symbol version)))
    (cl-qrencode:encode-png fp
                            :fpath "function-pattern.png"
                            :pixsize 9
                            :margin 8)))

(defun raw-symbol-png (text &key (version 1) (level :level-m) (mode nil))
  "dump raw symbol into png"
  (let ((raw (raw-symbol text :version version :level level :mode mode)))
    (cl-qrencode:encode-png raw
                            :fpath "raw-symbol.png"
                            :pixsize 9
                            :margin 8)))
