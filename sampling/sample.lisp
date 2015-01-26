;;;; Sample a function.

(in-package :soundlab.sampling)

(defvar *sample-rate* nil
  "During sampling, this variable is bound to an integer corresponding to
the sampling frequency in hertz.")

(defun n-samples (length frequency)
  "Number of samples required for sampling LENGTH seconds at a rate of
FREQUENCY in hertz."
  (nth-value 0 (floor (* length frequency))))

(defmacro with-samples ((x-var n-var length frequency) &body loop-forms)
  "Loop over X-VAR and N-VAR bound to time and sample index values
according to LENGTH and FREQUENCY while LOOP-FROMS are appended to the
loop body.  Use this for any sampling activities!"
  `(let ((*sample-rate* ,frequency))
     (loop for ,x-var from 0 to ,length by (float (/ 1 frequency))
           for ,n-var upto (1- (n-samples length frequency))
          ,@loop-forms)))

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

(defun make-sample-buffer-signal (channel rate)
  "Make signal for CHANNEL at RATE."
  (let ((length (length channel)))
    (lambda (x)
      (let ((n (n-samples x rate)))
        (if (>= n length) 0
            (aref channel n))))))

(defun sample-buffer-signals (sample-buffer)
  "Return signals for SAMPLE-BUFFER."
  (apply #'values
         (loop for channel across (sample-buffer%-channels sample-buffer)
            collect (make-sample-buffer-signal
                     channel
                     (sample-buffer%-rate sample-buffer)))))
