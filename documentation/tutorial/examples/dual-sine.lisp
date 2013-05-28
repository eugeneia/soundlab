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
