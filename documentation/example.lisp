
;;;; Examples for using SOUNDLAB.

;; Like CL-USER for SOUNDLAB.
(in-package :soundlab-user)

(set-tempo 60) ; 60 bpm

(defun o4 (note)
  "Octave with A4 at 440Hz."
  (octave 440 note))

(defun o2 (note)
  "Octave with A4 at 110Hz."
  (octave 110 note))

(defun dual-sine (note)
  "Two aligned sines."
  (chord (sine (o4 note))
         (sine (o2 note))))

(defun hoover (signal)
  "Adding slightly detuned copies of SIGNAL makes it hoover."
  (chord signal
         (pitch signal 0.983)
         (pitch signal 0.997)
         (pitch signal 1.019)
         (pitch signal 1.013)))

(defun stage-1 (note)
  "Combines DUAL-SINE and HOOVER."
  (hoover (dual-sine note)))


(defun voice-envelope ()
  "Envelope for our hoover signal.
See figures/voice-envelope.png."
  (envelope 0.01 1/32-note 0.3 1/16-note))

(defun stage-2 (trigger note)
  "Apply VOICE-ENVELOPE to STAGE-1."
  (envelope* (stage-1 note)
             (voice-envelope)
             trigger))


(defun sequence (voice events)
  "A simple sequencer.
EVENT  := (TIMING TRIGGER NOTE)
TIMING := (4TH 16TH 64TH)"
  (flet ((timing-shift (timing)
           (destructuring-bind (i4 &optional (i16 0) (i64 0))
               timing
             (+ (* i4 1/4-note)
                (* i16 1/16-note)
                (* i64 (/ 1/32-note 2)))))
         (event-signal (trigger note-sym)
           (funcall voice (* note trigger) note-sym)))
    (apply #'chord
           (loop for (timing trigger note-sym) in events
              collect (shift (event-signal trigger note-sym)
                             (timing-shift timing))))))

(defun stage-3 ()
  "Use SEQUENCE to play our voice from STAGE-2."
  (sequence
   #'stage-2
   '(((0 0) 1/2 :g#) ((0 1) 1/64 :d) ((0 2) 1/64 :d)  ((0 3) 1/16 :g#)
     ((1 0) 1/8 :g#) ((1 1) 1/4 :d)  ((1 2) 1/64 :d)  ((1 3) 1/16 :g#)
     ((2 0) 1/2 :g#) ((2 1) 1/64 :d) ((2 2) 1/64 :d#) ((2 3) 1/16 :d)
     ((3 0) 1/8 :c)  ((3 1) 1/4 :c)  ((3 2) 1/64 :g)  ((3 3) 1/16 :g))))


(defun phaser (signal mix offset)
  "Improvised phaser effect."
  (mix* signal
        (pitch* signal (add offset (flatline 1)))
        mix))

(defun phaser-envelope ()
  "An envelope for our phaser.
See figures/phaser-envelope.png"
  (repeat-envelope
   (envelope 1/8-note 1/32-note 0.2 1/2-note
             :attack-curve #'square
             :decay-curve #'square)
   1/2-note
   note
   (interval-frequency (* 2 note))))

(defun stage-4 ()
  "Apply phaser effect to score from STAGE-3."
  (phaser (repeat (stage-3) note)
          (flatline 0.5)
          (multiply (phaser-envelope)
                    (flatline 0.001))))

