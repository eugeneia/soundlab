;;;; Combinator utilities.

(in-package :soundlab.combinators)

(defun function-ratio (function)
  "Return ratio function (0:1) for signal function (-1:1)."
  (divide (add function (flatline 1))
          (flatline 2)))
