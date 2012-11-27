;;;; Sample a function.

(in-package :soundlab.sampling)

(defun n-samples (length frequency)
  "Number of samples required for sampling LENGTH seconds at a rate of
FREQUENCY in hertz."
  (nth-value 0 (floor (* length frequency))))

(defmacro! for-sample (x-var n-var o!length o!frequency
		       &body loop-body)
  "Looping construct speciaized for sampling."
  `(loop for ,x-var from 0 to ,g!length by (/ 1 ,g!frequency)
         for ,n-var upto (1- (n-samples ,g!length ,g!frequency))
	,@loop-body))

(defun sample-function (function length frequency)
  "Sample FUNCTION from zero to LENGTH with FREQUENCY."
  (let ((array (make-array (n-samples length frequency)
			   :element-type 'cons)))
    (for-sample x n length frequency
      do (setf (aref array n)
	       (cons x (funcall function x))))
    array))
