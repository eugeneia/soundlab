;;;; Sine wave.

(in-package :soundlab.waves)

(defun sine (frequency)
  "Sine wave at FREQUENCY."
  (lambda (x) (sin (* 2 pi frequency x))))
