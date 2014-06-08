;;;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-qrencode)

(deftype qr-mode ()
  '(member :unknown
    :numeric :alnum :byte :kanji
    ;; Extended Channel Interpretation, Structured Append, FNC1
    :eci :structured :fnc1))

(defun mode-indicator (mode)
  (declare (type qr-mode mode))
  (case mode
    (:numeric '(0 0 0 1)) ; "0001"
    (:alnum '(0 0 1 0))   ; "0010"
    (:byte '(0 1 0 0))    ; "0100"
    (:kanji '(1 0 0 0))   ; "1000"
    (:eci '(0 1 1 1))     ; "0111"
    (:structured '(0 0 1 1)) ; "0011"
    (:fnc1 '(0 1 0 1))))  ; FIXME: "0101" & "1001"

(defun terminator (bstream version level)
  "End of message"
  (let* ((nbits (length bstream))
         (diff (- (* (data-words-capacity version level) 8)
                  nbits)))
    (cond
      ((< diff 0) (error "you serious about this?!"))
      ((<= diff 4) (make-list diff :initial-element 0))
      (t (make-list 4 :initial-element 0)))))

(defun byte-value (mode byte)
  "BYTE value under MODE"
  (declare (type qr-mode mode))
  (case mode
    (:numeric
     (and (<= #x30 byte #x39)
          (- byte #x30)))
    (:alnum
     (cond
       ((<= #x30 byte #x39) (- byte #x30)) ; 0-9
       ((<= #x41 byte #x5A) (+ (- byte #x41) 10)) ; A-Z
       ((= byte #x20) 36) ; SP
       ((= byte #x24) 37) ; $
       ((= byte #x25) 38) ; %
       ((= byte #x2A) 39) ; *
       ((= byte #x2B) 40) ; +
       ((= byte #x2D) 41) ; -
       ((= byte #x2E) 42) ; .
       ((= byte #x2F) 43) ; /
       ((= byte #x3A) 44) ; :
       (t nil)))
    ((:byte :kanji) byte)))

(defun kanji-word-p (word)
  "(kanji-p, kanji-range: {0, 1})"
  (cond
    ((<= #x8140 word #x9ffc) (values t 0))
    ((<= #xe040 word #xebbf) (values t 1))
    (t (values nil nil))))

(defun starts-kanji-p (bytes)
  "(BYTES starts with kanji-p, kanji word value, kanji-range: {0, 1})"
  (declare (type list bytes))
  (let* ((first (car bytes))
         (second (cadr bytes))
         (word (and second (+ (ash first 8) second))))
    (if (and first second)
        (multiple-value-bind (kanji-p range)
            (kanji-word-p word)
          (values kanji-p word range))
        (values nil nil nil))))

(defun xor-subset-of (bytes)
  "exclusive subset of first unit of BYTES.
as for unit, one byte for :numeric, :alnum; two bytes for :kanji"
  (declare (type list bytes))
  (let* ((first (car bytes)))
    (cond
      ((null first) :unknown)
      ((byte-value :numeric first) :numeric)
      ((byte-value :alnum first) :alnum)
      ;; excluding reserved values 80-9F & E0-FF
      ((and (not (<= #x80 first #x9F))
            (not (<= #xE0 first #xFF)))
       :byte)
      ((starts-kanji-p bytes)
       :kanji))))
