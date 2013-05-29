changecom
define(`notes', `ifdef(`slides',, `include(`annotations/$1.mk10')')')


 notes(`introduction')

 notes(`structure')


< HOW DOES THIS WORK?

 #media The workflow.#
 figures/workflow.png


 notes(`workflow')

 
 < Whats a signal?

  The sum of sines, e.g.

  #media Sine.#
  figures/sine.png

  #media Sine noise.#
  figures/sine-noise.png

  A function which maps time to amplitude (-1..1) e.g. {#'SIN}.

 >


 < Recording sound

  #media Sampling simplified.#
  figures/sample.png


  notes(`sample')

 >


 notes(`sample-test')

>

< SIGNAL COMBINATORS

 #code Implementing {CHORD}.#
 (defun chord (&rest signals)

   (lambda (x)
     (/ (loop for signal in signals
           sum (funcall signal x))
        (length signals)))

   (divide (apply #'add signals)
           (flatline (length signals)))
 #

 #code A common interface for time and amplitude modulation.#
 (FUNCTION (&REST (FUNCTION (REAL) REAL))
           (FUNCTION (REAL) REAL))
 #


 notes(`signal-combinators')

>


< OVERVIEW OF THE LIBRARY

 #media Library overview.#
 figures/soundlab-overview.png


 notes(`soundlab-overview')

>


< USING SOUNDLAB

 #media What we are going to do.#
 ../tutorial/figures/sketch.png

 #media Voice envelope.#
 ../tutorial/figures/voice-envelope-8th.png

 #media Phaser envelope.#
 ../tutorial/figures/phaser-envelope.png

>
