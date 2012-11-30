;;;; Chord combinator (normalizing ADD)

(in-package :soundlab.combinators)

(defun chord (&rest functions)
  "Add and normalize FUNCTIONS."
  (unless functions
    (error "You need to supply at least one function to CHORD."))
  (divide (apply #'add functions)
	  (flatline (length functions))))