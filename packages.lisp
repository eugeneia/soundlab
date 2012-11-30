;;;; Package definitions for SOUNDLAB.

(defpackage :soundlab.sampling
  (:documentation
   "Procedures for sampling functions with support for exporting WAVE
files and Gnuplot compatible data files.")
  (:use :cl
	:riff-wave.write
	:defmacro!)
  (:export :sample-function
	   :sample-function-graph
	   :export-function-graph
	   :sample-function-wave
	   :export-function-wave))

(defpackage :soundlab.waves
  (:documentation
   "Procedures which generate wave forms.")
  (:use :cl)
  (:export :sine
	   :binary
	   :sawtooth
	   :flatline))

(defpackage :soundlab.notes
  (:documentation
   "Procedures which implement a simple western note system.")
  (:use :cl)
  (:export :shift-note
	   :octave))

(defpackage :soundlab.tempo
  (:documentation
   "Dynamic timing variables and procedures to convert tempo units.")
  (:use :cl)
  (:export :bpm-quarter-note
	   :interval-frequency
	   :set-tempo
	   :note :note-if
	   :1/2-note :1/2-note-if
	   :1/4-note :1/4-note-if
	   :1/8-note :1/8-note-if
	   :1/16-note :1/16-note-if
	   :1/32-note :1/32-note-if))

(defpackage :soundlab.envelope
  (:documentation
   "Envelope implementation.")
  (:use :cl)
  (:export :envelope
	   :linear
	   :square))

(defpackage :soundlab.combinators
  (:documentation
   "I have no idea what I am doing. I thought I knew but boy am I lost.")
  (:use :cl
	:soundlab.waves)
  (:export :add
	   :subtract
	   :multiply
	   :divide
	   :chord
	   :dampen
	   :pitch
	   :pitch*
	   :repeat
	   :envelope*))

(defpackage :soundlab-user
  (:documentation
   "User package that interns all soundlab packages.")
  (:use :cl
	:soundlab.sampling
	:soundlab.waves
	:soundlab.notes
	:soundlab.tempo
	:soundlab.envelope
	:soundlab.combinators))
