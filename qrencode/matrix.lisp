;;;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Codeword placement in matrix

(in-package #:cl-qrencode)

(deftype module-color ()
  ":RAW, nothing has been done to this module; :RESERVE, format info reserve module
:FLIGHT/:FDARK, function pattern light/dark module; :LIGHT/:DARK, data modules"
  '(member :raw :flight :fdark :reserve :light :dark))

(deftype matrix ()
  `(simple-array module-color (* *)))

(defun same-color-p (color1 color2)
  "during QR symbol evaluation, :fdark & :dark are considered to be same"
  (case color1
    ((:flight :light) (or (eq color2 :flight) (eq color2 :light)))
    ((:fdark :dark) (or (eq color2 :fdark) (eq color2 :fdark)))
    (otherwise (eq color1 color2))))

(defun raw-module-p (matrix i j)
  "nothing has been done to MATRIX[I, J]"
  (declare (type matrix matrix))
  (eq (aref matrix i j) :raw))

(declaim (ftype (function (fixnum &optional module-color) matrix) make-modules-matrix))
(defun make-modules-matrix (modules &optional (init :raw))
  "make a raw matrix with MODULES * MODULES elements"
  (make-array `(,modules ,modules) :initial-element init))

(defun make-matrix (version &optional (init :raw))
  "make a raw matrix according to VERSION"
  (let ((n (matrix-modules version)))
    (make-modules-matrix n init)))

(defun paint-square (matrix x y n &optional (color :fdark))
  "Paint a square of size N*N starting from upleft (X, Y) in MATRIX to COLOR"
  (declare (type matrix matrix))
  (let ((maxx (+ x n -1))
        (maxy (+ y n -1)))
    (loop for i from x to maxx do
         (loop for j from y to maxy do
              (setf (aref matrix i j) color))))
  matrix)

;;; Function Patterns
(defun function-patterns (matrix version)
  (let ((modules (matrix-modules version)))
    (finder-patterns matrix modules)
    (separator matrix modules)
    (timing-patterns matrix modules)
    (alignment-patterns matrix version))
  matrix)
;; a) Finder Patterns: fixed position in matrix
(defun one-finder-pattern (matrix x y)
  "Paint one finder pattern starting from upleft (X, Y)"
  (paint-square matrix x y 7 :fdark)
  (paint-square matrix (+ x 1) (+ y 1) 5 :flight)
  (paint-square matrix (+ x 2) (+ y 2) 3 :fdark))
(defun finder-patterns (matrix modules)
  ;; top-left finder pattern
  (one-finder-pattern matrix 0 0)
  ;; top-right finder pattern
  (one-finder-pattern matrix (- modules 7) 0)
  ;; bottom-left finder pattern
  (one-finder-pattern matrix 0 (- modules 7)))

;; b) Separator: fixed position in matrix
(defun separator (matrix modules)
  (dotimes (j 8)
    ;; top-left horizontal separator
    (setf (aref matrix 7 j) :flight)
    ;; top-right horizontal separator
    (setf (aref matrix 7 (- modules j 1)) :flight)
    ;; bottom-left horizontal separator
    (setf (aref matrix (- modules 8) j) :flight))
  (dotimes (i 8)
    ;; top-left vertical separator
    (setf (aref matrix i 7) :flight)
    ;; bottom-left vertical separator
    (setf (aref matrix (- modules i 1) 7) :flight)
    ;; top-right vertical separator
    (setf (aref matrix i (- modules 8)) :flight))
  matrix)

;; c) Timing patterns
(defun timing-patterns (matrix modules)
  (let ((color :fdark))
    (loop for idx from 8 to (- modules 9) do
         (if (evenp idx)
             (setf color :fdark)
             (setf color :flight))
         ;; Horizontal
         (setf (aref matrix 6 idx) color)
         ;; Vertical
         (setf (aref matrix idx 6) color)))
  matrix)

;; d) Alignment Patterns: varies between versions
;; may overlap timing patterns, modules coincide with that of timing patterns
(defun one-align-pattern (matrix x y)
  "Paint one alignment pattern centered at (X, Y)"
  (paint-square matrix (- x 2) (- y 2) 5 :fdark)
  (paint-square matrix (- x 1) (- y 1) 3 :flight)
  (paint-square matrix x y 1 :fdark))
(defun alignment-patterns (matrix version)
  (dolist (center (align-centers version) matrix)
    (one-align-pattern matrix (first center) (second center))))

;;; Encoding Region
(defun symbol-character (bstream matrix version)
  (let ((modules (matrix-modules version)))
    (reserve-information matrix version)
    (bstream-placement bstream matrix modules))
  matrix)
;; reserve format information & version information
(defun reserve-information (matrix version)
  (let ((modules (matrix-modules version)))
    ;; format information...
    ;; top-left & top-right horizontal
    (dotimes (j 8)
      (when (raw-module-p matrix 8 j)
        (setf (aref matrix 8 j) :reserve))
      (setf (aref matrix 8 (- modules j 1)) :reserve))
    (setf (aref matrix 8 8) :reserve)
    ;; top-left & bottom-left vertical
    (dotimes (i 8)
      (when (raw-module-p matrix i 8)
        (setf (aref matrix i 8) :reserve))
      (setf (aref matrix (- modules i 1) 8) :reserve))
    ;; dark module...
    (setf (aref matrix (- modules 8) 8) :fdark)

    ;; version information for version 7-40
    (when (>= version 7)
      (version-information matrix modules version))))

(defun paint-fcolor-bit (matrix i j bit)
  "Paint function pattern color for MATRIX[I, J] according to BIT of {0, 1}"
  (setf (aref matrix i j) (case bit
                            (0 :flight) (1 :fdark))))
(defun version-information (matrix modules version)
  "version information placement on two blocks of modules:
bottom-left 3*6 block: [modules-11, modules-9] * [0, 5]
top-right 6*3 block:   [0, 5] * [modules-11, modules-9]"
  (assert (>= version 7))
  (let ((vib (version-ecc version))
        (i (- modules 9))
        (start (- modules 9))
        (bound (- modules 11))
        (j 5))
    (dolist (bit vib matrix)
      (paint-fcolor-bit matrix i j bit)
      (paint-fcolor-bit matrix j i bit)
      (if (>= (- i 1) bound)
          (decf i)
          (progn
            (decf j)
            (setf i start))))))

;; Symbol character placement
(defun paint-color-bit (matrix i j bit)
  "Paint data color for MATRIX[I, J] according to BIT of {0, 1}"
  (setf (aref matrix i j) (case bit
                            (0 :light) (1 :dark))))
(defun bstream-placement (bstream matrix modules)
  "2X4 module block for a regular symbol character. Regard the interleaved
codeword sequence as a single bit stream, which is placed in the two module
wide columns, alternately in the right and left modules, moving upwards or
downwards according to DIRECTION, skipping function patterns, changing DIRECTION
at the top or bottom of the symbol. The only exception is that no block should
ever overlap the vertical timing pattern."
  (declare (type matrix matrix)
           (type list bstream))
  (loop with i = (1- modules)
        with j = (1- modules)
        ;; -1: upwards, +1: downwards
        with direction = -1
        ;;
        while bstream
        ;;
        if (raw-module-p matrix i j)
        do (paint-color-bit matrix i j (pop bstream))
        ;;
        if (and (>= (- j 1) 0)
                (raw-module-p matrix i (- j 1)))
        ;; try left module
        do (paint-color-bit matrix i (- j 1) (pop bstream))
        ;;
        do (if (< -1 (+ i direction) modules)
          (incf i direction)
          (progn
            ;; reverse direction
            (setf direction (- direction))
            (if (= j 8)
                ;; vertical timing pattern reached, the next block starts
                ;; to the left of it
                (decf j 3)
                (decf j 2)))))
    matrix)

;;; format information, during and after masking
(defun format-information (matrix modules level mask-ind)
  ;; format information bistream
  (let ((fib (format-ecc level mask-ind))
        (darks 0)
        (idx 0)
        (idx2 0))
    (setf darks (count-if #'(lambda (elem) (= elem 1)) fib))
    ;; horizontal 14 ~ 8
    (loop for j from 0 to 7 do
         (when (eq (aref matrix 8 j) :reserve)
           (paint-fcolor-bit matrix 8 j (nth idx fib))
           (incf idx)))
    ;; vertical 14 ~ 8
    (loop for i from (- modules 1) downto (- modules 7) do
         (paint-fcolor-bit matrix i 8 (nth idx2 fib))
         (incf idx2))
    ;; horizontal 7 - 0
    (loop for j from (- modules 8) to (- modules 1) do
         (paint-fcolor-bit matrix 8 j (nth idx fib))
         (incf idx))
    ;; vertical 7 - 0
    (loop for i from 8 downto 0 do
         (when (eq (aref matrix i 8) :reserve)
           (paint-fcolor-bit matrix i 8 (nth idx2 fib))
           (incf idx2)))
    (values matrix darks)))
