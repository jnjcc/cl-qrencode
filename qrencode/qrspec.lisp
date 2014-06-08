;;;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-qrencode)

;;; Table 1 - Codeword capacity of all versions of QR Code 2005
;;; excluding Micro QR Code, varies between version
(defvar *codeword-capacity-table*
  #2A((-1  -1   -1 -1    -1   -1) ; 0, no such version
      (21  202  31 208   26   0) (25  235  31 359   44   7)
      (29  243  31 567   70   7) (33  251  31 807   100  7)
      (37  259  31 1079  134  7) (41  267  31 1383  172  7)
      (45  390  67 1568  196  0) (49  398  67 1936  242  0)
      (53  406  67 2336  292  0) (57  414  67 2768  346  0) ; Version 10
      (61  422  67 3232  404  0) (65  430  67 3728  466  0)
      (69  438  67 4256  532  0) (73  611  67 4651  581  3)
      (77  619  67 5243  655  3) (81  627  67 5867  733  3)
      (85  635  67 6523  815  3) (89  643  67 7211  901  3)
      (93  651  67 7931  991  3) (97  659  67 8683  1085 3) ; Version 20
      (101 882  67 9252  1156 4) (105 890  67 10068 1258 4)
      (109 898  67 10916 1364 4) (113 906  67 11796 1474 4)
      (117 914  67 12708 1588 4) (121 922  67 13652 1706 4)
      (125 930  67 14628 1828 4) (129 1203 67 15371 1921 3)
      (133 1211 67 16411 2051 3) (137 1219 67 17483 2185 3) ; Version 30
      (141 1227 67 18587 2323 3) (145 1235 67 19723 2465 3)
      (149 1243 67 20891 2611 3) (153 1251 67 22091 2761 3)
      (157 1574 67 23008 2876 0) (161 1582 67 24272 3034 0)
      (165 1590 67 25568 3196 0) (169 1598 67 26896 3362 0)
      (173 1606 67 28256 3532 0) (177 1614 67 29648 3706 0)) ; Version 40
  "Number of modules (as version increases, 4 modules added) A | Function pattern
modules B | Format and Version information modules C | Data modules (A^2-B-C) |
Data capacity codewords (bytes, including ecc codewords) | Remainder bits.")
(defun codeword-capacity (version)
  "codeword: data word + ecc word"
  (aref *codeword-capacity-table* version 4))
(defun matrix-modules (version)
  (aref *codeword-capacity-table* version 0))
(defun remainder-bits (version)
  (aref *codeword-capacity-table* version 5))

(defun mode->index (mode)
  (case mode
    (:numeric 0)
    (:alnum 1)
    (:byte 2)
    (:kanji 3)))

(deftype ecc-level ()
  '(member :level-l :level-m :level-q :level-h))
(defun level->index (level)
  (case level
    (:level-l 0)
    (:level-m 1)
    (:level-q 2)
    (:level-h 3)))

;;; (Part I of) Table 9 - Number of Error Correction Codewords (bytes)
;;; varies between version and level
(defvar *ecc-codewords-table*
  ;; (:level-l :level-m :level-q :level-h)
  #2A((-1  -1   -1   -1) ;; 0, no such version
      (7   10   13   17)   (10  16   22   28)   (15  26   36   44)
      (20  36   52   64)   (26  48   72   88)   (36  64   96   112)
      (40  72   108  130)  (48  88   132  156)  (60  110  160  192)
      (72  130  192  224)  (80  150  224  264)  (96  176  260  308)
      (104 198  288  352)  (120 216  320  384)  (132 240  360  432)
      (144 280  408  480)  (168 308  448  532)  (180 338  504  588)
      (196 364  546  650)  (224 416  600  700)  (224 442  644  750)
      (252 476  690  816)  (270 504  750  900)  (300 560  810  960)
      (312 588  870  1050) (336 644  952  1110) (360 700  1020 1200)
      (390 728  1050 1260) (420 784  1140 1350) (450 812  1200 1440)
      (480 868  1290 1530) (510 924  1350 1620) (540 980  1440 1710)
      (570 1036 1530 1800) (570 1064 1590 1890) (600 1120 1680 1980)
      (630 1204 1770 2100) (660 1260 1860 2220) (720 1316 1950 2310)
      (750 1372 2040 2430))) ;; version 1 ~ 40
(defun ecc-words-capacity (version level)
  (aref *ecc-codewords-table* version (level->index level)))
(defun data-words-capacity (version level)
  (- (codeword-capacity version) (ecc-words-capacity version level)))

;;; (Part II of) Table 9 - Error Correction blocks
;;; varies between version and level
(defvar *ecc-blocks*
  ;; (version, level) =>
  ;;   (# of ec codewords for each blk, # of blk 1, # of data words for blk 1,
  ;;                                    # of blk 2, # of data words for blk 2)
  ;; :level-l :level-m :level-q :level-h
  #3A(((0  0 0  0 0)     (0  0 0  0 0)    (0  0 0  0 0)    (0  0 0 0 0))     ; no such version
      ((7  1 19 0 0)     (10 1 16 0 0)    (13 1 13 0 0)    (17 1 9 0 0))     ; Version 1
      ((10 1 34 0 0)     (16 1 28 0 0)    (22 1 22 0 0)    (28 1 16 0 0))
      ((15 1 55 0 0)     (26 1 44 0 0)    (18 2 17 0 0)    (22 2 13 0 0))
      ((20 1 80 0 0)     (18 2 32 0 0)    (26 2 24 0 0)    (16 4 9 0 0))
      ((26 1 108 0 0)    (24 2 43 0 0)    (18 2 15 2 16)   (22 2 11 2 12))   ; Version 5
      ((18 2 68 0 0)     (16 4 27 0 0)    (24 4 19 0 0)    (28 4 15 0 0))
      ((20 2 78 0 0)     (18 4 31 0 0)    (18 2 14 4 15)   (26 4 13 1 14))
      ((24 2 97 0 0)     (22 2 38 2 39)   (22 4 18 2 19)   (26 4 14 2 15))
      ((30 2 116 0 0)    (22 3 36 2 37)   (20 4 16 4 17)   (24 4 12 4 13))
      ((18 2 68 2 69)    (26 4 43 1 44)   (24 6 19 2 20)   (28 6 15 2 16))   ; Version 10
      ((20 4 81 0 0)     (30 1 50 4 51)   (28 4 22 4 23)   (24 3 12 8 13))
      ((24 2 92 2 93)    (22 6 36 2 37)   (26 4 20 6 21)   (28 7 14 4 15))
      ((26 4 107 0 0)    (22 8 37 1 38)   (24 8 20 4 21)   (22 12 11 4 12))
      ((30 3 115 1 116)  (24 4 40 5 41)   (20 11 16 5 17)  (24 11 12 5 13))
      ((22 5 87 1 88)    (24 5 41 5 42)   (30 5 24 7 25)   (24 11 12 7 13))  ; Version 15
      ((24 5 98 1 99)    (28 7 45 3 46)   (24 15 19 2 20)  (30 3 15 13 16))
      ((28 1 107 5 108)  (28 10 46 1 47)  (28 1 22 15 23)  (28 2 14 17 15))
      ((30 5 120 1 121)  (26 9 43 4 44)   (28 17 22 1 23)  (28 2 14 19 15))
      ((28 3 113 4 114)  (26 3 44 11 45)  (26 17 21 4 22)  (26 9 13 16 14))
      ((28 3 107 5 108)  (26 3 41 13 42)  (30 15 24 5 25)  (28 15 15 10 16)) ; Version 20
      ((28 4 116 4 117)  (26 17 42 0 0)   (28 17 22 6 23)  (30 19 16 6 17))
      ((28 2 111 7 112)  (28 17 46 0 0)   (30 7 24 16 25)  (24 34 13 0 0))
      ((30 4 121 5 122)  (28 4 47 14 48)  (30 11 24 14 25) (30 16 15 14 16))
      ((30 6 117 4 118)  (28 6 45 14 46)  (30 11 24 16 25) (30 30 16 2 17))
      ((26 8 106 4 107)  (28 8 47 13 48)  (30 7 24 22 25)  (30 22 15 13 16)) ; Version 25
      ((28 10 114 2 115) (28 19 46 4 47)  (28 28 22 6 23)  (30 33 16 4 17))
      ((30 8 122 4 123)  (28 22 45 3 46)  (30 8 23 26 24)  (30 12 15 28 16))
      ((30 3 117 10 118) (28 3 45 23 46)  (30 4 24 31 25)  (30 11 15 31 16))
      ((30 7 116 7 117)  (28 21 45 7 46)  (30 1 23 37 24)  (30 19 15 26 16))
      ((30 5 115 10 116) (28 19 47 10 48) (30 15 24 25 25) (30 23 15 25 16)) ; Version 30
      ((30 13 115 3 116) (28 2 46 29 47)  (30 42 24 1 25)  (30 23 15 28 16))
      ((30 17 115 0 0)   (28 10 46 23 47) (30 10 24 35 25) (30 19 15 35 16))
      ((30 17 115 1 116) (28 14 46 21 47) (30 29 24 19 25) (30 11 15 46 16))
      ((30 13 115 6 116) (28 14 46 23 47) (30 44 24 7 25)  (30 59 16 1 17))
      ((30 12 121 7 122) (28 12 47 26 48) (30 39 24 14 25) (30 22 15 41 16)) ; Version 35
      ((30 6 121 14 122) (28 6 47 34 48)  (30 46 24 10 25) (30 2 15 64 16))
      ((30 17 122 4 123) (28 29 46 14 47) (30 49 24 10 25) (30 24 15 46 16))
      ((30 4 122 18 123) (28 13 46 32 47) (30 48 24 14 25) (30 42 15 32 16))
      ((30 20 117 4 118) (28 40 47 7 48)  (30 43 24 22 25) (30 10 15 67 16))
      ((30 19 118 6 119) (28 18 47 31 48) (30 34 24 34 25) (30 20 15 61 16)) ; Version 40
      ))
(defun ecc-block-nums (version level)
  "# of ec codewords for each blk, # of blk 1, # of data words for blk 1, ..."
  (let ((lidx (level->index level)))
    (values (aref *ecc-blocks* version lidx 0)
            (aref *ecc-blocks* version lidx 1)
            (aref *ecc-blocks* version lidx 2)
            (aref *ecc-blocks* version lidx 3)
            (aref *ecc-blocks* version lidx 4))))

(defun minimum-version (init-version nbytes level)
  "minimum version that can hold NBYTES data words, or INIT-VERSION if bigger"
  (do ((v init-version (1+ v)))
      ((> v 40) nil)
    (when (>= (data-words-capacity v level) nbytes)
      (return-from minimum-version v))))

(defun version-range (version)
  (cond
    ((<= 1 version 9) 0)
    ((<= 10 version 26) 1)
    ((<= 27 version 40) 2)))

;;; Table 3 - Number of bits in character count indicator for QR Code 2005
(defvar *char-count-indicator*
  ;; :numeric :alnum :byte :kanji
  #2A((10 9  8  8)    ; version-range 0
      (12 11 16 10)   ; version-range 1
      (14 13 16 12))) ; version-range 2
(defun char-count-bits (version mode)
  (let ((i (version-range version))
        (j (mode->index mode)))
    (aref *char-count-indicator* i j)))

;;; Table E.1 - Row/column coordinates of center modules of alignment patterns
;;; varies between versions
(defvar *align-coord-table*
  #2A((0  ()) ; 0, no such version
      (0  ())                       (1  (6 18))                   (1  (6 22))
      (1  (6 26))                   (1  (6 30))                   (1  (6 34))
      (6  (6 22 38))                (6  (6 24 42))                (6  (6 26 46))
      (6  (6 28 50))                (6  (6 30 54))                (6  (6 32 58))
      (6  (6 34 62))                (13 (6 26 46 66))             (13 (6 26 48 70))
      (13 (6 26 50 74))             (13 (6 30 54 78))             (13 (6 30 56 82))
      (13 (6 30 58 86))             (13 (6 34 62 90))             (22 (6 28 50 72 94))
      (22 (6 26 50 74 98))          (22 (6 30 54 78 102))         (22 (6 28 54 80 106))
      (22 (6 32 58 84 110))         (22 (6 30 58 86 114))         (22 (6 34 62 90 118))
      (33 (6 26 50 74 98 122))      (33 (6 30 54 78 102 126))     (33 (6 26 52 78 104 130))
      (33 (6 30 56 82 108 134))     (33 (6 34 60 86 112 138))     (33 (6 30 58 86 114 142))
      (33 (6 34 62 90 118 146))     (46 (6 30 54 78 102 126 150)) (46 (6 24 50 76 102 128 154))
      (46 (6 28 54 80 106 132 158)) (46 (6 32 58 84 110 136 162)) (46 (6 26 54 82 110 138 166))
      (46 (6 30 58 86 114 142 170)))
  "# of Alignment Patterns, row/column coordinates of center modules.")
(defun valid-center-p (x y modules)
  "The alignment center module is not in Finder Patterns."
  (not (or (and (<= 0 x 8) (<= 0 y 8)) ; upleft finder pattern
           (and (<= 0 x 8)
                (<= (- modules 8) y (- modules 1))) ; upright finder pattern
           (and (<= (- modules 8) x (- modules 1))
                (<= 0 y 8)))))
(defun align-centers (version)
  "list of all valid alignment pattern center modules under VERSION"
  (let* ((modules (matrix-modules version))
         (coords (aref *align-coord-table* version 1))
         (len (length coords))
         (centers nil))
    (dotimes (i len)
      (loop for j from i to (- len 1) do
           (let ((x (nth i coords))
                 (y (nth j coords)))
             (when (valid-center-p x y modules)
               (push (list x y) centers))
             (unless (= x y)
               (when (valid-center-p y x modules)
                 (push (list y x) centers))))))
    centers))

(defvar *mask-pattern-num* 8)
(defun mask-condition (indicator)
  (lambda (i j)
    (case indicator
      ;; (i + j) mod 2 == 0
      (0 (= (mod (+ i j) 2) 0))
      ;; i mod 2 == 0
      (1 (= (mod i 2) 0))
      ;; j mod 3 == 0
      (2 (= (mod j 3) 0))
      ;; (i + j) mod 3 == 0
      (3 (= (mod (+ i j) 3) 0))
      ;; ((i/2) + (j/3)) mod 2 == 0
      (4 (= (mod (+ (floor i 2) (floor j 3)) 2) 0))
      ;; (i*j) mod 2 + (i*j) mod 3 == 0
      (5 (= (+ (mod (* i j) 2) (mod (* i j) 3)) 0))
      ;; ((i*j) mod 2 + (i*j) mod 3)) mod 2 == 0
      (6 (= (mod (+ (mod (* i j) 2) (mod (* i j) 3)) 2) 0))
      ;; ((i+j) mod 2 + (i*j) mod 3)) mod 2 == 0
      (7 (= (mod (+ (mod (+ i j) 2) (mod (* i j) 3)) 2) 0)))))

(defvar *ecc-level-indicator* #((0 1) (0 0) (1 1) (1 0))
  ":level-l :level-m :level-q :level-h")
(defun level-indicator (level)
  (aref *ecc-level-indicator* (level->index level)))
(defvar *mask-pattern-reference*
  #((0 0 0) (0 0 1) (0 1 0) (0 1 1)
    (1 0 0) (1 0 1) (1 1 0) (1 1 1)))
(defun mask-pattern-ref (ind)
  (aref *mask-pattern-reference* ind))
