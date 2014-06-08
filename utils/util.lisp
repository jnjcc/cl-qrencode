;;;; Copyright (c) 2011-2014 jnjcc, Yste.org, all rights reserved.
;;;;

(in-package #:cl-qrencode)

;;; Adapted from P. Norvig's PAIP
(defvar *dbg-ids* nil)
(defun sdebug (&rest ids)
  (setf *dbg-ids* (union ids *dbg-ids*)))
(defun undebug (&rest ids)
  (setf *dbg-ids* (if (null ids)
                      nil
                      (set-difference *dbg-ids* ids))))
(defun dbg (id format-string &rest args)
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun read-file-content (fpath)
  (with-open-file (fp fpath)
    (let ((content (make-string (file-length fp))))
      (read-sequence content fp)
      content)))