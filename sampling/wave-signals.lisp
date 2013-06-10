;;;; Import signal from WAVE file.

(in-package :soundlab.sampling)

(defstruct sample-buffer%
  "Buffer struct for imported samples."
  (rate (error "Must specifify RATE.") :type unsigned-integer)
  (channels nil :type (array (array real))))

(defun make-sample-buffer (rate n-channels length)
  "Return SAMPLE-BUFFER% struct for RATE, N-CHANNELS and LENGTH."
  (let ((channels (make-array n-channels :element-type '(array real))))
    (loop for i from 0 to (1- n-channels)
       do (setf (aref channels i)
                (make-array length :element-type 'real)))
    (make-sample-buffer% :rate rate :channels channels)))

(defun set-sample (sample-buffer sample channel value)
  "Set VALUE for SAMPLE / CHANNEL in SAMPLE-BUFFER."
  (setf (aref (aref (sample-buffer%-channels sample-buffer) channel)
              sample)
        value))

(defun sample-buffer-signals (sample-buffer)
  "Return signals for SAMPLE-BUFFER."
  (apply #'values
         (loop for channel across (sample-buffer%-channels sample-buffer)
            collect (let ((ch channel)) ; lexic bind
                      (lambda (x)
                        (aref ch (floor (* x (sample-buffer%-rate
                                              sample-buffer)))))))))

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
