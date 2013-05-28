changecom
define(`notes', `ifdef(`slides',, `include(`annotations/$1.mk10')')')

< Hi

 my name is Max Rottenkolber.

 I will talk about *exploratory sound synthesis using SOUNDLAB*.


 notes(`introduction')

>


< Structure of this demo

 #table Table of contents.#
 | *I*   | HOW DOES THIS WORK?
 | *II*  | SIGNAL COMBINATION
 | *III* | OVERVIEW OF THE LIBRARY
 | *IV*  | USING {SOUNDLAB}


 notes(`structure')

>


< *I* HOW DOES THIS WORK?

 #media The workflow.#
 figures/workflow.png


 notes(`workflow')

>


< Whats a signal?

 The sum of sines, e.g.

 #media Sine.#
 figures/sine.png

 or

 #media Sine noise.#
 figures/sine-noise.png

 For our purposes: A function which maps time to amplitude (-1 - 1),
 e.g. {#'SIN}.


>


< Recording sound

 #media Sampling simplified.#
 figures/sample.png


 notes(`sample')

>


notes(`sample-test')


< *III* OVERVIEW OF THE LIBRARY

 #media Library overview.#
 figures/soundlab-overview.png


 notes(`soundlab-overview')

>


< *IV* USING {SOUNDLAB}

 #media What we're gonna do.#
 ../tutorial/figures/sketch.png

>


< Voice envelope

 #media Voice envelope.#
 ../tutorial/figures/voice-envelope.png

>


< Phaser envelope

 #media Phaser envelope.#
 ../tutorial/figures/phaser-envelope.png

>
