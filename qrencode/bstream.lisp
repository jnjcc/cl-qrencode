;;;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; bit stream (a list of 0-1 values) utilities

(in-package #:cl-qrencode)

(defun decimal->bstream (dec nbits)
  "using NBITS bits to encode decimal DEC"
  (let ((bstream nil))
    (dotimes (i nbits)
      (if (logbitp i dec)
          (push 1 bstream)
          (push 0 bstream)))
    bstream))
(defun bstream->decimal (bstream nbits)
  (declare (type list bstream))
  (let ((nbits (min nbits (length bstream)))
        (dec 0))
    (dotimes (i nbits)
      (setf dec (+ (* dec 2) (nth i bstream))))
    dec))

;;; :numeric mode
(defun group->decimal (values ndigits)
  "digit groups of length NDIGITS (1, 2 or 3) to decimal"
  (declare (type list values))
  (case ndigits
    (1 (nth 0 values))
    (2 (+ (* (nth 0 values) 10) (nth 1 values)))
    (3 (+ (* (nth 0 values) 100) (* (nth 1 values) 10) (nth 2 values)))))
(defun final-digit-bits (n)
  "the final one or two digits are converted to 4 or 7 bits respectively"
  (case n
    (0 0) (1 4) (2 7)))
(defun numeric->bstream (bytes)
  (declare (type list bytes))
  (labels ((num-value (byte)
             (byte-value :numeric byte)))
    (let ((values (mapcar #'num-value bytes))
          (bstream nil))
      (do ((v values (nthcdr 3 v)))
          ((null v) bstream)
        (case (length v)
          (1 ; only 1 digits left
           (setf bstream
                 (append bstream (decimal->bstream (group->decimal v 1)
                                                   (final-digit-bits 1)))))
          (2 ; only 2 digits left
           (setf bstream
                 (append bstream (decimal->bstream (group->decimal v 2)
                                                   (final-digit-bits 2)))))
          (otherwise ; at least 3 digits left
           (setf bstream
                 (append bstream
                         (decimal->bstream (group->decimal v 3) 10)))))))))

;;; :alnum mode
(defun pair->decimal (values num)
  "alnum pairs of length NUM (1 or 2) to decimal"
  (declare (type list values))
  (case num
    (1 (nth 0 values))
    (2 (+ (* (nth 0 values) 45) (nth 1 values)))))
(defun alnum->bstream (bytes)
  (declare (type list bytes))
  (labels ((alnum-value (byte)
             (byte-value :alnum byte)))
    (let ((values (mapcar #'alnum-value bytes))
          (bstream nil))
      (do ((v values (nthcdr 2 v)))
          ((null v) bstream)
        (case (length v)
          (1 ; only 1 alnum left
           (setf bstream
                 (append bstream
                         (decimal->bstream (pair->decimal v 1) 6))))
          (otherwise ; at least 2 alnum left
           (setf bstream
                 (append bstream
                         (decimal->bstream (pair->decimal v 2) 11)))))))))

;;; :byte mode
(defun byte->bstream (bytes)
  (declare (type list bytes))
  (labels ((join (prev cur)
             (append prev (decimal->bstream (byte-value :byte cur) 8))))
    (reduce #'join bytes :initial-value nil)))

;;; :kanji mode
(defun kanji->decimal (word range)
  (let ((subtractor (ecase range
                      (0 #x8140)
                      (1 #xc140))))
    (decf word subtractor)
    (setf word (+ (* (ash word -8) #xc0)
                  (boole boole-and word #xff)))))
(defun kanji->bstream (bytes)
  (declare (type list bytes))
  (labels ((kanji-value (byte)
             (byte-value :kanji byte)))
    (let ((values (mapcar #'kanji-value bytes))
          (delta 1)
          (bstream nil))
      (do ((v values (nthcdr delta v)))
          ((null v) bstream)
        (case (length v)
          (1 ; only 1 byte left
           (setf bstream
                 (append bstream (decimal->bstream (car v) 13)))
           (setf delta 1))
          (otherwise ; at least 2 bytes left
           (multiple-value-bind (kanji-p word range) (starts-kanji-p v)
             (if kanji-p
                 (progn
                   (setf bstream
                         (append bstream
                                 (decimal->bstream (kanji->decimal word range)
                                                   13)))
                   (setf delta 2))
                 (progn
                   (setf bstream
                         (append bstream (decimal->bstream (car v) 13)))
                   (setf delta 1))))))))))

;;; :eci mode
(defun eci->bstream (bytes)
  "TODO"
  (declare (ignore bytes))
  (error "eci->bstream: TODO..."))

(defun bstream-trans-func (mode)
  (case mode
    (:numeric #'numeric->bstream)
    (:alnum #'alnum->bstream)
    (:byte #'byte->bstream)
    (:kanji #'kanji->bstream)))

(defun kanji-bytes-length (bytes)
  (declare (type list bytes))
  (let ((step 1)
        (len 0))
    (do ((b bytes (nthcdr step b)))
        ((null b) len)
      (if (starts-kanji-p b)
          (setf step 2)
          (setf step 1))
      (incf len))))

(defun bytes-length (bytes mode)
  "number of data characters under MODE"
  (declare (type list bytes) (type qr-mode mode))
  (case mode
    ((:numeric :alnum :byte) (length bytes))
    (:kanji (kanji-bytes-length bytes))))

(defun segment-bstream-length (segment version)
  "bit stream length of SEGMENT (:mode b0 b1 ...) under VERSION"
  (declare (type list segment))
  (let* ((mode (car segment))
         (bytes (cdr segment))
         (m 4)
         (c (char-count-bits version mode))
         (d (bytes-length bytes mode))
         (r 0))
    ;; M = number of bits in mode indicator
    ;; C = number of bits in character count indicator
    ;; D = number of input data characters
    (case mode
      (:numeric
       (setf r (final-digit-bits (mod d 3)))
       ;; B = M + C + 10 * (D / 3) + R
       (+ m c (* 10 (floor d 3)) r))
      (:alnum
       (setf r (mod d 2))
       ;; B = M + C + 11 * (D / 2) + 6 * (D % 2)
       (+ m c (* 11 (floor d 2)) (* 6 r)))
      (:byte
       ;; B = M + C + 8 * D
       (+ m c (* 8 d)))
      (:kanji
       ;; B = M + C + 13 * D
       (+ m c (* 13 d))))))

(defun segment->bstream (segment version)
  "SEGMENT (:mode b0 b1 ...) to bit stream under VERSION"
  (declare (type list segment))
  (let* ((mode (car segment))
         (bytes (cdr segment))
         (len (bytes-length bytes mode))
         (n (char-count-bits version mode))
         (bstream nil))
    (append bstream (mode-indicator mode)
            (decimal->bstream len n) ; character count indicator
            (funcall (bstream-trans-func mode) bytes))))
