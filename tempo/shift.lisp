;;;; Shift signals in phase.

(in-package :soundlab.tempo)

(defun shift (signal offset)
  "Shift SIGNAL in time by OFFSET."
  (lambda (x)
    (funcall signal (- x offset))))
