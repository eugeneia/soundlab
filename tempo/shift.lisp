;;;; Shift signals in phase.

(in-package :soundlab.tempo)

(defun shift (signal offset)
  "Shift SIGNAL in time by OFFSET. Return amplitude of zero for resulting
values of time less than zero."
  (lambda (x)
    (let ((xo (- x offset)))
      (if (>= xo 0)
          (funcall signal xo)
          0))))
