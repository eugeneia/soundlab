(defun hoover (signal)
  "Adding slightly detuned copies of SIGNAL makes it `hoover'."
  (chord signal
         (pitch signal 0.983)
         (pitch signal 0.997)
         (pitch signal 1.019)
         (pitch signal 1.013)))
