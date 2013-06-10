;;;; Mix signals.

(in-package :soundlab.combinators)

(defun mix* (function-a function-b ratio-function)
  "Mix FUNCTION-A and FUNCTION-B according to RATIO-FUNCTION (0-1)."
  (add (multiply function-a (subtract (flatline 1) ratio-function))
       (multiply function-b ratio-function)))

(defun mix (function-a function-b ratio)
  "Mix FUNCTION-A and FUNCTION-B with RATIO (0-1)."
  (mix* function-a function-b (flatline ratio)))

(defun stereo-mix* (signal ratio-function)
  "Split SIGNAL up to two signals as by RATIO-FUNCTION (0-1)."
  (values (mix* signal (flatline 0) ratio-function)
          (mix* (flatline 0) signal ratio-function)))

(defun stereo-mix (signal ratio)
  "Split SIGNAL up to two signals as by RATIO (0-1)."
  (stereo-mix* signal (flatline ratio)))
