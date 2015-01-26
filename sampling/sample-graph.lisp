;;;; Export sampling results to Gnuplot compatible graph data files.

(in-package :soundlab.sampling)

(defparameter *graph-output-format* "~8f ~8f~%"
  "Gnuplot compatible representation for samples.")

(defun sample-graph (function length frequency stream)
  "Sample FUNCTION from zero to LENGTH with FREQUENCY and write it to
STREAM as a Gnuplot compatible data file."
  (with-samples (x n length frequency)
    do (format stream *graph-output-format* n (funcall function x))))

(defun export-graph (function length frequency path
			      &key (if-exists :error))
  "Direct output of SAMPLE-GRAPH to PATH. If PATH already exists use
IF-EXISTS to control behaviour, default is :ERROR."
  (with-open-file (out path
		       :direction :output
		       :if-exists if-exists)
    (sample-graph function length frequency out)))
