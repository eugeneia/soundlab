;;;; Obvious arithmetic operations.

(in-package :soundlab.combinators)

(defun add (&rest functions)
  "Add the results of FUNCTIONS."
  (if functions
      (lambda (x)
	(loop for function in functions
	   sum (funcall function x)))
      (lambda (x)
	(declare (ignore x))
	0)))

(defun subtract (function &rest other-functions)
  "Add the results of OTHER-FUNCTIONS and subtract them from the result
of FUNCTION."
  (lambda (x)
    (- (funcall function x)
       (funcall (apply #'add other-functions) x))))

(defun multiply (&rest functions)
  "Multiply the results of FUNCTIONS."
  (lambda (x)
    (let ((product 1))
      (dolist (function functions)
	(setf product (* product (funcall function x))))
      product)))

(defun divide (function &rest other-functions)
  "Multiply the results of OTHER-FUNCTIONS and divide the result of
FUNCTION by their product."
  (lambda (x)
    (/ (funcall function x)
       (funcall (apply #'multiply other-functions) x))))
