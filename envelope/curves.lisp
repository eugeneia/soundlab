;;;; Curves to use with ENVELOPE.

(in-package :soundlab.envelope)

(defun linear (x)
  "Linear curve. Just return X untouched."
  x)

(defun square (x)
  "Square curve."
  (expt x 2))