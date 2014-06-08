;;;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-qrencode-test)

(define-test validate-align-centers
  (loop for v from 1 to 40 do
       (let ((centers (cl-qrencode::align-centers v)))
         (assert-eql (aref cl-qrencode::*align-coord-table* v 0)
                     (length centers)))))

(define-test validate-align-version7
  (let ((expected '((6 22) (22 6) (22 22) (22 38) (38 22) (38 38)))
        (centers (cl-qrencode::align-centers 7)))
    (labels ((cmp (a b)
               (cond
                 ((= (car a) (car b)) (< (cadr a) (cadr b)))
                 (t (< (car a) (car b))))))
      (setf centers (sort centers #'cmp))
      (assert-equal expected centers))))