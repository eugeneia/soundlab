;;;; ADSR envelope.
;;;;
;;;;  /\___
;;;; /   | \
;;;; -------
;;;; | | | |
;;;; | | | Release
;;;; | | Sustain
;;;; | Decay
;;;; Attack

(in-package :soundlab.envelope)


;;; Some auxillary functions

(defun curve (curve x maximum)
  "Apply CURVE on X relative to MAXIMUM."
  (funcall curve (/ x maximum)))

(defun invert (x)
  "Invert X."
  (- 1 x))


;;; The actual envelope factory

(defun envelope (attack decay sustain release
		 &key (attack-curve #'linear)
		      (decay-curve #'linear)
                      (release-curve #'linear))
  "Return ADSR envelope function. ATTACK, DECAY and RELEASE are values in
seconds, SUSTAIN is a value ranging from zero to one. The -CURVE
paramaters are functions that shape the envelopes slopes. They should map
linear input ranging from zero to one to an arbitrary curve and default
to LINEAR."
  (lambda (trigger)
    ;; Attack, decay and release slopes
    (labels ((attack (x)
	       (curve attack-curve x attack))
	     (decay (x)
	       (+ (* (invert (curve decay-curve
				    (- x attack)
				    decay))
		     (invert sustain))
		  sustain))
	     (release (x)
	       (* (invert (curve release-curve
				 (- x trigger)
				 release))
		  (cond ((<= trigger attack)
			 (attack trigger))
			((<= trigger (+ attack decay))
			 (decay trigger))
			(t sustain)))))
      ;; Envelope objects returns graph for TRIGGER time
      (lambda (x)
	(if (<= x trigger)
	    (cond ((<= x attack) (attack x))
		  ((<= x (+ attack decay)) (decay x))
		  (t sustain))
	    (cond ((<= x (+ trigger release)) (release x))
		  (t 0)))))))
