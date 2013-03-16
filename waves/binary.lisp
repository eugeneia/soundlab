;;;; Square wave.

(in-package :soundlab.waves)

(defun binary (frequency)
  "Square wave at FREQUENCY."
  (let ((sine (sine frequency)))
    (lambda (x) (if (> (funcall sine x) 0) 1 -1))))
      
      