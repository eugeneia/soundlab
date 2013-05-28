(defun sequence (voice events)
  "A simple sequencer.
EVENT  := (TIMING TRIGGER NOTE)
TIMING := (4TH 16TH 64TH)"
  (flet ((timing-shift (timing)
           (destructuring-bind (i4 &optional (i16 0) (i64 0))
               timing
             (+ (* i4 1/4-note)
                (* i16 1/16-note)
                (* i64 (/ 1/32-note 2)))))
         (event-signal (trigger note-sym)
           (funcall voice (* note trigger) note-sym)))
    (apply #'chord
           (loop for (timing trigger note-sym) in events
              collect (shift (event-signal trigger note-sym)
                             (timing-shift timing))))))
