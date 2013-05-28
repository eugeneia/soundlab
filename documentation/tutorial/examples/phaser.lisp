(defun phaser (signal mix offset)
  "Improvised phaser effect."
  (mix* signal
        (pitch* signal (add offset (flatline 1)))
        mix))
