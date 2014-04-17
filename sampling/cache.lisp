;;;; Cache a function for fast reuse.

(in-package :soundlab.sampling)

(defun cache (function sample-rate length)
  "Sample LENGTH seconds of FUNCTION with SAMPLE-RATE and capture the
signal in a buffer. Return a signal which plays back the captured
buffer."
  (let* ((n-samples (ceiling (* sample-rate length)))
         (sample-buffer (make-sample-buffer sample-rate 1 n-samples)))
    (dotimes (sample n-samples)
      (set-sample sample-buffer sample 0
                  (funcall function (/ sample sample-rate))))
    (sample-buffer-signals sample-buffer)))
