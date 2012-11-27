;;;; Flat line.

(in-package :soundlab.waves)

(defun flatline (y)
  "Always return Y."
  (lambda (x)
    (declare (ignore x))
    y))
