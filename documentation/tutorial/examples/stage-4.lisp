(defun phaser-envelope ()
  "An envelope for our phaser.
See `figures/phaser-envelope.png'"
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
