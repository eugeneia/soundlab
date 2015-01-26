;;;; Import signal from WAVE file.

(in-package :soundlab.sampling)

(defgeneric wave-signals (source)
  (:documentation "Return signal for WAVE data from SOURCE."))

(defmethod wave-signals ((source stream))
  (multiple-value-bind (sample-rate sample-size n-channels length)
      (read-wave-header source)
    ;; Make a buffer for samples from SOURCE.
    (let ((sample-buffer
           (make-sample-buffer sample-rate n-channels length)))
      ;; Read in samples <ERROR>.
      (dotimes (sample length)
        ;; Read in channels for sample.
        (dotimes (channel n-channels)
          (set-sample sample-buffer sample channel
                      (read-sample source sample-size))))
      ;; Return signals.
      (sample-buffer-signals sample-buffer))))

(defmethod wave-signals ((source pathname))
  (with-open-file (in source :element-type '(unsigned-byte 8))
    (wave-signals in)))

(defmethod wave-signals ((source string))
  (wave-signals (pathname source)))
