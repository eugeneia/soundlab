(defun stage-3 ()
  "Use SEQUENCE to play our voice from STAGE-2."
  (sequence
   #'stage-2
   '(((0 0) 1/2 :g#) ((0 1) 1/64 :d) ((0 2) 1/64 :d)  ((0 3) 1/16 :g#)
     ((1 0) 1/8 :g#) ((1 1) 1/4 :d)  ((1 2) 1/64 :d)  ((1 3) 1/16 :g#)
     ((2 0) 1/2 :g#) ((2 1) 1/64 :d) ((2 2) 1/64 :d#) ((2 3) 1/16 :d)
     ((3 0) 1/8 :c)  ((3 1) 1/4 :c)  ((3 2) 1/64 :g)  ((3 3) 1/16 :g))))
