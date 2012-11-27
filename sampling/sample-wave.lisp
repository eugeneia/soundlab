;;;; Export sampling results to WAVE file.

(in-package :soundlab.sampling)

(defun sample-function-wave (function length frequency sample-size
			     stream)
  "Sample FUNCTION from zero to LENGTH with FREQUENCY and SAMPLE-SIZE and
write it to STREAM as a WAVE."
  (write-wave-header frequency
		     sample-size
		     1
		     (n-samples length frequency)
		     stream)
  (for-sample x n length frequency
    do (write-sample (funcall function x) sample-size stream)))

(defun export-function-wave (function length path
			     &key (frequency 44100)
			          (sample-size 2)
			          (if-exists :error))
  "Direct output of SAMPLE-FUNCTION-WAVE to PATH. If PATH already exists
use IF-EXISTS to control behaviour, default is :ERROR."
  (with-open-file (out path
		       :element-type 'unsigned-byte
		       :direction :output
		       :if-exists if-exists)
    (sample-function-wave function length frequency sample-size out)))
