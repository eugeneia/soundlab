;;;; Export sampling results to WAVE file.

(in-package :soundlab.sampling)

(defparameter *sample-frequency-default* 44100
  "Default value for FREQUENCY keyword parameter.")

(defparameter *sample-size-default* 2
  "Default value for SAMPLE-SIZE keyword parameter.")

(defparameter *export-if-exists-default* :error
  "Default value for IF-EXISTS keyword parameter.")

(defun sample-wave* (signals length frequency sample-size stream)
  "Sample SIGNALS from zero to LENGTH with FREQUENCY and SAMPLE-SIZE and
write it to STREAM as a WAVE-file."
  (write-wave-header frequency
		     sample-size
		     (length signals)
		     (n-samples length frequency)
		     stream)
  (with-samples (x n length frequency)
    do (loop for signal in signals do
            (write-sample (funcall signal x) sample-size stream))))

(defun sample-wave (signal length frequency sample-size stream)
  "Sample SIGNALS from zero to LENGTH with FREQUENCY and SAMPLE-SIZE and
write it to STREAM as a WAVE-file."
  (sample-wave* (list signal) length frequency sample-size stream))

(defun export-wave* (signals length path
                     &key (frequency *sample-frequency-default*)
                          (sample-size *sample-size-default*)
                          (if-exists *export-if-exists-default*))
  "Direct output of SAMPLE-WAVE* to PATH. If PATH already exists use
IF-EXISTS to control behaviour, default is :ERROR."
  (with-open-file (out path
		       :element-type 'unsigned-byte
		       :direction :output
		       :if-exists if-exists)
    (sample-wave* signals length frequency sample-size out)))

(defun export-wave (signal length path
                    &key (frequency *sample-frequency-default*)
                         (sample-size *sample-size-default*)
                         (if-exists *export-if-exists-default*))
  "Direct output of SAMPLE-WAVE to PATH. If PATH already exists use
IF-EXISTS to control behaviour, default is :ERROR."
  (export-wave* (list signal) length path
                :frequency frequency
                :sample-size sample-size
                :if-exists if-exists))
