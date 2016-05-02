;;;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Bose-Chaudhuri-Hocquenghem (BCH) error correction code

(in-package #:cl-qrencode)

;;; Polynomial (using list) arithmetics
;;; by polynomial list (3 2 1), we mean 3*x^2 + 2*x + 1
(defun poly-ash (poly s)
  "shift left POLY by S"
  (declare (type list poly))
  (append poly (make-list s :initial-element 0)))
(defun poly-multiply (poly b &optional (op #'*))
  "multiply B on every element of POLY using OP"
  (labels ((mult (elem)
             (funcall op elem b)))
    (mapcar #'mult poly)))
(defun poly-substract (lhs rhs &optional (op #'-))
  (labels ((sub (elem1 elem2)
             (funcall op elem1 elem2)))
    (mapcar #'sub lhs rhs)))
(defun poly-mod (msg gen rem &optional (sub #'poly-substract) (mul #'poly-multiply))
  "MSG % GEN, with REM remainders"
  (labels ((cdrnzero (msg)
             (do ((head msg (cdr head)))
                 ((or (null head) (/= (car head) 0)) head)
               head)))
    (do ((m (poly-ash msg rem) (cdrnzero m)))
        ((<= (length m) rem) m)
      (let* ((glen (length gen))
             (sft (- (length m) glen))
             ;; LEAD coffiecient of message polynomial
             (lead (car m)))
        (setf m (funcall sub m (poly-ash (funcall mul gen lead) sft)))))))

(defclass bch-ecc ()
  ((k :initform nil :initarg :k
      :documentation "# of data codewords")
   (ec :initform nil :initarg :ec
       :documentation "# of error correction codewords")))

(defun bch* (poly b)
  (poly-multiply poly b))
(defun bch- (lhs rhs)
  (labels ((xor (a b)
             (boole boole-xor a b)))
    (poly-substract lhs rhs #'xor)))
(defun bch-xor (lhs rhs)
  (labels ((xor (a b)
             (boole boole-xor a b)))
    (mapcar #'xor lhs rhs)))
(defun bch% (msg gen rem)
  (poly-mod msg gen rem #'bch- #'bch*))

(defgeneric bch-ecc (bch msgpoly genpoly)
  (:documentation "do bch error correction under BCH(K+EC, K)"))

(defmethod bch-ecc ((bch bch-ecc) msg gen)
  (with-slots (k ec) bch
    (unless (= (length msg) k)
      (error "wrong msg length, expect: ~A; got: ~A~%" k (length msg)))
    (bch% msg gen ec)))

;;; As used by format information ecc & version information ecc respectively
;;; BCH(15, 5) & BCH(18, 6)
(let ((fi-ecc (make-instance 'bch-ecc :k 5 :ec 10))
      ;; format information generator polynomial
      ;; x^10 + x^8 + x^5 + x^4 + x^2 + x + 1
      (fi-gpoly '(1 0 1 0 0 1 1 0 1 1 1))
      (fi-xor '(1 0 1 0 1 0 0 0 0 0 1 0 0 1 0)))
  (defun format-ecc (level mask-ind)
    (let ((seq (append (level-indicator level)
                       (mask-pattern-ref mask-ind))))
      (bch-xor (append seq (bch-ecc fi-ecc seq fi-gpoly))
               fi-xor))))

(let ((vi-ecc (make-instance 'bch-ecc :k 6 :ec 12))
      ;; version information generator polynomial
      ;; x^12 + x^11 + x^10 + x^9 + x^8 + x^5 + x^2 + 1
      (vi-gpoly '(1 1 1 1 1 0 0 1 0 0 1 0 1)))
  (defun version-ecc (version)
    (let ((seq (decimal->bstream version 6)))
      (append seq (bch-ecc vi-ecc seq vi-gpoly)))))
