;;;; Package definitions for SOUNDLAB.

(defpackage :soundlab.sampling
  (:documentation
   "Procedures for sampling functions with support for exporting WAVE
files and Gnuplot compatible data files.")
  (:use :cl
	:riff-wave.write
        :riff-wave.read)
  (:export :sample-function
	   :sample-graph
	   :export-graph
	   :sample-wave
	   :sample-wave*
	   :export-wave
	   :export-wave*
           :wave-signals))

(defpackage :soundlab.waves
  (:documentation
   "Procedures which generate wave forms.")
  (:use :cl)
  (:export :sine
	   :binary
;	   :sawtooth
	   :flatline))

(defpackage :soundlab.combinators
  (:documentation
   "I have no idea what I am doing. I thought I knew but boy am I
  lost. Still, all this appears to be something good!")
  (:use :cl
	:soundlab.waves)
  (:export :add
	   :subtract
	   :multiply
	   :divide
	   :chord
	   :mix
	   :mix*
           :stereo-mix
           :stereo-mix*
	   :dampen
	   :pitch
	   :pitch*
	   :repeat
	   :envelope*
           :duck
           :gate))

(defpackage :soundlab.envelope
  (:documentation
   "Implementation of Attack-Decay-Sustain-Release envelopes.")
  (:use :cl)
  (:export :envelope
	   :linear
	   :square))

(defpackage :soundlab.tempo
  (:documentation
   "Special timing variables and procedures to convert tempo units.")
  (:use :cl)
  (:export :bpm-quarter-note
	   :interval-frequency
	   :tempo-offset
           :shift
	   :set-tempo
	   :note :note-if
	   :1/2-note :1/2-note-if
	   :1/4-note :1/4-note-if
	   :1/8-note :1/8-note-if
	   :1/16-note :1/16-note-if
	   :1/32-note :1/32-note-if))

(defpackage soundlab.waves.envelope
  (:documentation
   "Wave form from envelope.")
  (:use :cl
	:soundlab.tempo
	:soundlab.combinators)
  (:export :repeat-envelope
	   :envelope-wave))

(defpackage soundlab.filters
  (:documentation
   "Filters for functions.")
  (:use :cl)
  (:export :lowslope))

(defpackage :soundlab.notes
  (:documentation
   "Procedures that implement a simple western note system.")
  (:use :cl)
  (:export :shift-note
	   :octave))

(defpackage :soundlab-user
  (:documentation
   "User package that interns all soundlab packages.")
  (:use :cl
	:soundlab.sampling
	:soundlab.waves
	:soundlab.combinators
	:soundlab.envelope
	:soundlab.tempo
	:soundlab.waves.envelope
	:soundlab.filters
	:soundlab.notes))
