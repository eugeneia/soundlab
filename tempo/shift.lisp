;;;; Shift signals in phase.

(in-package :soundlab.tempo)

(defun shift (signal offset)
  (lambda (x)
    (let ((xo (- x offset)))
      (if (>= xo 0)
          (funcall signal xo)
          0))))
