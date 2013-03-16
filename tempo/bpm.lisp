;;;; Functions to calculate time spans in seconds based on tempo in BPM.

(in-package :soundlab.tempo)

(defun bpm-quarter-note (tempo)
  "Return length of quarter note at TEMPO."
  (/ 60 tempo))

(defun interval-frequency (interval)
  "Return frequency for INTERVAL."
  (/ 1 interval))

;; Dynamic variables for note lengths and interval frequencies (if)
(defvar note         nil "Length of a note.")
(defvar note-if      nil "Interval frequency for a note.")
(defvar 1/2-note     nil "Length of a half note.")
(defvar 1/2-note-if  nil "Interval frequency for a half note.")
(defvar 1/4-note     nil "Length of a quarter note.")
(defvar 1/4-note-if  nil "Interval frequency for a quarter note.")
(defvar 1/8-note     nil "Length of an eighth note.")
(defvar 1/8-note-if  nil "Interval frequency for an eighth note.")
(defvar 1/16-note    nil "Length of a 16th note.")
(defvar 1/16-note-if nil "Interval frequency for an 16th note.")
(defvar 1/32-note    nil "Length of a 32th note.")
(defvar 1/32-note-if nil "Interval frequency for an 32th note.")

(defmacro set-tempo (tempo)
  "Set time variables according to TEMPO and return value of NOTE."
  `(progn
     (setf 1/4-note     (bpm-quarter-note ,tempo)
	   1/4-note-if  (interval-frequency 1/4-note)
	   note         (* 1/4-note 4)
	   note-if      (interval-frequency note)
	   1/2-note     (* 1/4-note 2)
	   1/2-note-if  (interval-frequency 1/2-note)
	   1/8-note     (/ 1/4-note 2)
	   1/8-note-if  (interval-frequency 1/8-note)
	   1/16-note    (/ 1/4-note 4)
	   1/16-note-if (interval-frequency 1/16-note)
	   1/32-note    (/ 1/4-note 8)
	   1/32-note-if (interval-frequency 1/32-note))
     note))
