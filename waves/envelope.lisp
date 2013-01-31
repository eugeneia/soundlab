;;;; Waveform made from envelope.

(in-package :soundlab.waves.envelope)

(defun repeat-envelope (envelope trigger length frequency)
  "Repeat ENVELOPE shape triggered with TRIGGER for LENGTH at frequency."
  (let* ((shape (funcall envelope trigger))
	 (interval (interval-frequency frequency))
	 (scale (/ interval length)))
    (repeat (lambda (x) (funcall shape (/ x scale)))
	    interval)))

(defun envelope-wave (envelope trigger length frequency)
  "Return waveform at FREQUENCY made from ENVELOPE with TRIGGER and
LENGTH."
  (let ((r-envelope (repeat-envelope envelope trigger length frequency)))
    (lambda (x)
      (* 2 (- (funcall r-envelope x) 1/2)))))
