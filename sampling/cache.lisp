;;;; Cache a function for fast reuse.

(in-package :soundlab.sampling)

(defun cache (function frequency length)
  "Sample LENGTH seconds of FUNCTION with FREQUENCY and capture the
signal in a buffer. Return a signal which plays back the captured
buffer."
  (let ((buffer (make-sample-buffer
                 frequency 1 ; Mono.
                 (n-samples length frequency))))
    (with-samples (x n length frequency)
      do (set-sample buffer n 0 (funcall function x)))
    (sample-buffer-signals buffer)))
