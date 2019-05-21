;;;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; png backend for QR code symbol

(in-package #:cl-qrencode)

(defun put-black-square (arr x0 y0 size)
  (dotimes (y size)
    (let ((base (array-row-major-index arr (+ y0 y) x0 0)))
      (dotimes (x size)
        (setf (row-major-aref arr (+ x base)) 0)))))

(defun symbol->png (symbol pixsize margin)
  "return the qr symbol written into a zpng:png object with PIXSIZE
pixels for each module, and MARGIN pixels on all four sides"
  (declare (type fixnum pixsize margin))
  (with-slots (matrix modules) symbol
    (let* ((size (+ (* modules pixsize) (* margin 2)))
           (qrpng (make-instance 'zpng:png 
                                 :width size 
                                 :height size 
                                 :color-type :grayscale))
           (qrarray (zpng:data-array qrpng)))
      ;; Paint margins == quiet zone, everything becomes white.
      ;; TODO: should be done in zpng:png initialization directly!
      (dotimes (i (array-total-size qrarray))
        (setf (row-major-aref qrarray i) 
              255))
      ;;
      (dotimes (y modules)
        (dotimes (x modules)
          ;; It's sufficient to only write black pixels now
          (if (dark-module-p matrix x y)
            (put-black-square qrarray
                              (+ margin (* pixsize x))
                              (+ margin (* pixsize y))
                              pixsize))))
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
