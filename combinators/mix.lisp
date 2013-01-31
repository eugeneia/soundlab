;;;; Mix two functions with a specified ratio.

(in-package :soundlab.combinators)

(defun mix (function-a function-b ratio)
  "Mix FUNCTION-A and FUNCTION-B with RATIO (0-1)."
  (mix* function-a function-b (flatline ratio)))

(defun mix* (function-a function-b ratio-function)
  "MMix FUNCTION-A and FUNCTION-B according to RATIO-FUNCTION (0-1)."
  (add (multiply function-a (subtract (flatline 1) ratio-function))
       (multiply function-b ratio-function)))
