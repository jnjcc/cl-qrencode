;;;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; png backend for QR code symbol

(in-package #:cl-qrencode)

(defun set-color (pngarray x y color)
  (setf (aref pngarray x y 0) color)
  (setf (aref pngarray x y 1) color)
  (setf (aref pngarray x y 2) color))

(defun symbol->png (symbol pixsize margin)
  "return the qr symbol written into a zpng:png object with PIXSIZE
pixels for each module, and MARGIN pixels on all four sides"
  (declare (type fixnum pixsize margin))
  (with-slots (matrix modules) symbol
    (let* ((size (+ (* modules pixsize) (* margin 2)))
           (qrpng (make-instance 'zpng:png :width size :height size))
           (qrarray (zpng:data-array qrpng)))
      (dotimes (x size)
        (dotimes (y size)
          (if (and (<= margin x (- size margin 1))
                   (<= margin y (- size margin 1)))
              (let ((i (floor (- x margin) pixsize))
                    (j (floor (- y margin) pixsize)))
                (if (dark-module-p matrix i j)
                    (set-color qrarray x y 0)
                    (set-color qrarray x y 255)))
              ;; quiet zone
              (set-color qrarray x y 255))))
      qrpng)))

(defun encode-png (text &key (fpath "qrcode.png") (version 1) (level :level-m)
                   (mode nil) (pixsize 9) (margin 8))
  (let ((symbol (encode-symbol text :version version :level level :mode mode)))
    (zpng:write-png (symbol->png symbol pixsize margin) fpath)))

(defun encode-png-stream (text stream &key (version 1) (level :level-m)
                          (mode nil) (pixsize 9) (margin 8))
  (let ((symbol (encode-symbol text :version version :level level :mode mode)))
    (zpng:write-png-stream (symbol->png symbol pixsize margin) stream)))

(defun encode-png-bytes (bytes &key (fpath "kanji.png") (version 1)
                         (level :level-m) (mode nil) (pixsize 9) (margin 8))
  (let ((symbol (encode-symbol-bytes bytes :version version :level level
                                     :mode mode)))
    (zpng:write-png (symbol->png symbol pixsize margin) fpath)))

(defun encode-png-bytes-stream (bytes stream &key (version 1) (level :level-m)
                                (mode nil) (pixsize 9) (margin 8))
  (let ((symbol (encode-symbol-bytes bytes :version version :level level
                                     :mode mode)))
    (zpng:write-png-stream (symbol->png symbol pixsize margin) stream)))