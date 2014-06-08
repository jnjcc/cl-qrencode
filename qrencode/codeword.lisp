;;;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; bit stream to codeword conversion

(in-package #:cl-qrencode)

(defun padding-bits (bstream)
  "add padding bits so that BSTREAM ends at a codeword boundary"
  (make-list (- 8 (mod (length bstream) 8)) :initial-element 0))

(defun pad-codewords (bstream version level)
  "add pad codewords (after adding padding-bits) to fill data codeword capacity"
  (let ((pad-words '((1 1 1 0 1 1 0 0)
                     (0 0 0 1 0 0 0 1)))
        (pad-len (- (data-words-capacity version level)
                    (/ (length bstream) 8)))
        (ret nil))
    (dotimes (i pad-len)
      (setf ret (append ret (nth (mod i 2) pad-words))))
    ret))

(defun bstream->codewords (bstream)
  "convert bstream into codewords, as coefficients of the terms of a polynomial"
  (do ((b bstream (nthcdr 8 b))
       (codewords nil))
      ((null b) codewords)
    (setf codewords (append codewords (list (bstream->decimal b 8))))))

(defun take-in-turn (blks)
  "taking codewords from each block (bound by minimum length) in turn"
  (reduce #'append (apply #'mapcar #'list blks)))

(defun take-data-in-turn (blocks blk1 data1 blk2 data2)
  "taking data words from each block (might have different length) in turn"
  (let ((data-final nil)
        (left-blks nil))
    (setf data-final (take-in-turn blocks))
    (cond
      ((or (= blk1 0) (= blk2 0))
       ;; only one kind of block exists
       (setf left-blks nil))
      ((> data1 data2)
       ;; block 1 has more elements left
       (setf left-blks (mapcar #'(lambda (blk)
                                   (nthcdr data2 blk))
                               (subseq blocks 0 blk1))))
      ((> data2 data1)
       ;; block 2 has more elements left
       (setf left-blks (mapcar #'(lambda (blk)
                                   (nthcdr data1 blk))
                               (subseq blocks blk1 (+ blk1 blk2))))))
    (if left-blks
        (append data-final (take-in-turn left-blks))
        data-final)))
