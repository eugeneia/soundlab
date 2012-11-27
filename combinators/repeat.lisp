;;;; Repeat a function in an interval.

(in-package :soundlab.combinators)

(defun repeat (function interval)
  "Repeat FUNCTION in INTERVAL."
  (lambda (x)
    (funcall function (mod x interval))))
