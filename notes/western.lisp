;;;; Simple western note system.

(in-package :soundlab.notes)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *notes*
    '(:A (:A# :Bb) :B :C (:C# :Db)
      :D (:D# :Eb) :E :F (:F# :Gb)
      :G (:G# :Ab))
    "Note keywords."))

(defmacro map-note (note)
  "Map NOTE to half-steps."
  `(ecase ,note
     ,@(loop for step from 0 to 11
	     for note in *notes*
	  collect `(,note ,step))))

(defun shift-note (a-frequency half-steps)
  "Shift A-FREQUENCY by HALF-STEPS."
  (* (expt 2 (/ half-steps 12))
     a-frequency))

(defun octave (a-frequency note)
  "Return frequency for NOTE relative to A-FREQUENCY."
  (shift-note a-frequency (map-note note)))
