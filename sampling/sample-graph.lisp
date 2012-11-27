;;;; Export sampling results to Gnuplot compatible graph data files.

(in-package :soundlab.sampling)

(defconstant +graph-output-format+ "~8f ~8f~%"
  "Gnuplot compatible representation for samples.")

(defun sample-function-graph (function length frequency stream)
  "Sample FUNCTION from zero to LENGTH with FREQUENCY and write it to
STREAM as a Gnuplot compatible data."
  (let ((samples (sample-function function length frequency)))
    (loop for (x-value . y-value) across samples
       do (format stream +graph-output-format+ x-value y-value))
    samples))

(defun export-function-graph (function length frequency path
			      &key (if-exists :error))
  "Direct output of SAMPLE-FUNCTION-GRAPH to PATH. If PATH already exists
use IF-EXISTS to control behaviour, default is :ERROR."
  (with-open-file (out path
		       :direction :output
		       :if-exists if-exists)
    (sample-function-graph function length frequency out)))
