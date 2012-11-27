;;;; Dampen a function.

(in-package :soundlab.combinators)

(defun dampen (function dampening)
  "Dampen amplitude of FUNCTION by factor DAMPENING."
  (multiply function (flatline dampening)))
