(cl-qrencode:encode-png "01234567" :fpath "numer.png"
                        :version 1 :level :level-m)

(cl-qrencode:encode-png "HELLO WORLD" :fpath "alnum.png"
                        :version 1 :level :level-q)

(let ((text (cl-qrencode:read-file-content #p"README.md")))
  (cl-qrencode:encode-png (subseq text 0 200) :fpath "byte.png" :margin 12))

;;; normally, we choose not to encode :kanji
;;; in case you are interested, here's how we did it (it sucks...)
; (cl-qrencode:encode-png-bytes :fpath "kanji.png" '(#x93 #x5f #x26 #xe4 #xaa))
