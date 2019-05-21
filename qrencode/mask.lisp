;;;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Data masking

(in-package #:cl-qrencode)

;;; only encoding region modules (excluding format information) are masked
(defun encoding-module-p (matrix i j)
  "modules belong to encoding region, excluding format & version information"
  (member (aref matrix i j)
          '(:light :dark) 
          :test #'eq))

(defun non-mask-module-p (matrix i j)
  (not (encoding-module-p matrix i j)))

(defun reverse-module-color (matrix i j)
  (declare (type matrix matrix))
  (case (aref matrix i j)
    (:dark :light) (:light :dark)))

;;; all modules are evaluated:
;;;  there should be only :dark :light :fdark :flight modules left by now
(defun dark-module-p (matrix i j)
  (declare (type matrix matrix))
  (member (aref matrix i j) '(:dark :fdark)))

(defun copy-and-mask (matrix modules level mask-ind)
  "make a new matrix and mask using MASK-IND for later evaluation"
  (declare (type matrix matrix))
  (let ((ret (make-modules-matrix modules))
        (mask-p (mask-condition mask-ind))
        (darks 0))
    (dotimes (i modules)
      (dotimes (j modules)
        (cond
          ((non-mask-module-p matrix i j)
           (setf (aref ret i j) (aref matrix i j)))
          ((funcall mask-p i j) ; need mask
           (setf (aref ret i j) (reverse-module-color matrix i j)))
          (t
           (setf (aref ret i j) (aref matrix i j))))
        (when (dark-module-p ret i j)
          (incf darks))))
    (multiple-value-bind (dummy fi-darks)
        (format-information ret modules level mask-ind)
      (declare (ignore dummy))
      ;; add format information dark modules
      (values ret (+ darks fi-darks)))))

(defun mask-matrix (matrix modules level mask-ind)
  "do not evaluate, just go ahead and mask MATRIX using MASK-IND mask pattern"
  (declare (type matrix matrix))
  (let ((mask-p (mask-condition mask-ind)))
    (dotimes (i modules)
      (dotimes (j modules)
        (and (encoding-module-p matrix i j)
             (funcall mask-p i j)
             (setf (aref matrix i j) (reverse-module-color matrix i j)))))
    ;; paint format information
    (format-information matrix modules level mask-ind)
    matrix))

(defun choose-masking (matrix modules level)
  "mask and evaluate using each mask pattern, choose the best mask result"
  (declare (type matrix matrix))
  (let ((n4 10)
        (best-matrix nil)
        (mask-indicator nil)
        (min-penalty nil)
        (square (* modules modules))
        (cur-penalty 0))
    (dotimes (i *mask-pattern-num*)
      (multiple-value-bind (cur-matrix darks)
          (copy-and-mask matrix modules level i)
        ;; feature 4: proportion of dark modules in entire symbol
        (let ((bratio (/ (+ (* darks 200) square) square 2)))
          (setf cur-penalty (* (/ (abs (- bratio 50)) 5) n4)))
        (incf cur-penalty (evaluate-feature-123 cur-matrix modules))
        (when (or (null min-penalty)
                  (< cur-penalty min-penalty))
          (setf min-penalty cur-penalty
                mask-indicator i
                best-matrix cur-matrix))))
    (values best-matrix mask-indicator)))

;;; feature 1 & 2 & 3
(defun evaluate-feature-123 (matrix modules)
  (declare (type matrix matrix))
  (let ((penalty 0))
    (incf penalty (evaluate-feature-2 matrix modules))
    (dotimes (col modules)
      (let ((rlength (calc-run-length matrix modules col :row)))
        (incf penalty (evaluate-feature-1 rlength))
        (incf penalty (evaluate-feature-3 rlength))))
    (dotimes (row modules)
      (let ((rlength (calc-run-length matrix modules row :col)))
        (incf penalty (evaluate-feature-1 rlength))
        (incf penalty (evaluate-feature-3 rlength))))
    penalty))

(defun calc-run-length (matrix modules num &optional (direction :row))
  "list of number of adjacent modules in same color"
  (declare (type matrix matrix))
  (let ((rlength (make-array modules 
                             :element-type 'fixnum
                             :initial-element 0
                             :fill-pointer 0))
        (base (case direction
                (:row (* num modules))
                (:col num)))
        (step (case direction
                (:row 1)
                (:col modules)))
        (prev :dark))
    (labels 
        ((get-elem (idx)
           (row-major-aref matrix (+ base (* idx step))))
         (add-to-list (elem)
           (vector-push-extend elem rlength)))
      ;; we make sure index 1 is for dark module
      (when (same-color-p (get-elem 0) :dark)
        (add-to-list -1))
      (add-to-list 1)
      ;;
      (loop for i from 1 below modules
            for this = (get-elem i)
            do (cond
                 ((same-color-p this prev)
                  (incf 
                    (aref rlength 
                          (1- (fill-pointer rlength)))))
                 (T
                  (setf prev this)
                  (add-to-list 1))))
      rlength)))

(defun evaluate-feature-1 (rlength)
  "(5 + i) adjacent modules in row/column in same color. (N1 + i) points, N1 = 3"
  (declare (type (array fixnum) rlength))
  (let ((n1 3)
        (penalty 0))
    (map nil
         (lambda (sz)
           (when (> sz 5)
             (incf penalty (+ n1 sz -5))))
         rlength)
    penalty))

(defun evaluate-feature-3 (rlength)
  "1:1:3:1:1 ration (dark:light:dark:light:dark) pattern in row/column,
preceded or followed by light area 4 modules wide. N3 points, N3 = 40"
  (declare (type (array fixnum) rlength))
  (let ((n3 40)
        (len (length rlength))
        (penalty 0))
    (do ((i 3 (+ i 2)))
        ((>= i (- len 2)) penalty)
      (when (and (= (mod i 2) 1) ; for dark module
                 (= (mod (aref rlength i) 3) 0)
        (let ((fact (floor (aref rlength i) 3)))
          ;; 1:1:3:1:1
          (when (= fact
                   (aref rlength (- i 2))
                   (aref rlength (- i 1))
                   (aref rlength (+ i 1))
                   (aref rlength (+ i 2)))
            (cond
              ((<= (- i 3) 0) (incf penalty n3))
              ((>= (+ i 4) len) (incf penalty n3))
              ((>= (aref rlength (- i 3)) (* 4 fact)) (incf penalty n3))
              ((>= (aref rlength (+ i 3)) (* 4 fact)) (incf penalty n3))))))))))

(defun evaluate-feature-2 (matrix modules)
  "block m * n of modules in same color. N2 * (m-1) * (n-1) points, N2=3"
  (declare (type matrix matrix))
  (let ((n2 3)
        (penalty 0)
        (bcount 0))
    (dotimes (i (- modules 1) penalty)
      (dotimes (j (- modules 1))
        (when (dark-module-p matrix i j)
          (incf bcount))
        (when (dark-module-p matrix (+ i 1) j)
          (incf bcount))
        (when (dark-module-p matrix i (+ j 1))
          (incf bcount))
        (when (dark-module-p matrix (+ i 1) (+ j 1))
          (incf bcount))
        (when (or (= bcount 0) (= bcount 4))
          (incf penalty n2))))))
