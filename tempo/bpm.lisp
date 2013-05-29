;;;; Functions to calculate time spans in seconds based on tempo in BPM.

(in-package :soundlab.tempo)

(defun bpm-quarter-note (tempo)
  "Return length of quarter note at TEMPO."
  (/ 60 tempo))

(defun interval-frequency (interval)
  "Return frequency for INTERVAL."
  (/ 1 interval))

;; Global lexical variables for note lengths and interval frequencies.
(define-symbol-macro note         (symbol-value 'note))
(define-symbol-macro note-if      (symbol-value 'note-if))
(define-symbol-macro 1/2-note     (symbol-value '1/2-note))
(define-symbol-macro 1/2-note-if  (symbol-value '1/2-note-if))
(define-symbol-macro 1/4-note     (symbol-value '1/4-note))
(define-symbol-macro 1/4-note-if  (symbol-value '1/4-note-if))
(define-symbol-macro 1/8-note     (symbol-value '1/8-note))
(define-symbol-macro 1/8-note-if  (symbol-value '1/8-note-if))
(define-symbol-macro 1/16-note    (symbol-value '1/16-note))
(define-symbol-macro 1/16-note-if (symbol-value '1/16-note-if))
(define-symbol-macro 1/32-note    (symbol-value '1/32-note))
(define-symbol-macro 1/32-note-if (symbol-value '1/32-note-if))

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

(defun tempo-offset (from &optional (to note))
  "Calculate tempo offset of FROM and TO."
  (/ (- from to) to))

