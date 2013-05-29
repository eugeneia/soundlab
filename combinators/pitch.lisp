;;;; Pitch a function.

(in-package :soundlab.combinators)

(defun pitch* (function pitch-function)
  "Pitch FUNCTION with PITCH-FUNCTION."
  (lambda (x)
    (funcall function (* x (funcall pitch-function x)))))

(defun pitch (function pitch)
  "Pitch FUNCTION by PITCH."
  (pitch* function (flatline pitch)))
