;;;; Apply envelope to a function.

(in-package :soundlab.combinators)

(defun envelope* (function envelope trigger)
  "Apply ENVELOPE to FUNCTION with TRIGGER."
  (multiply function (funcall envelope trigger)))
