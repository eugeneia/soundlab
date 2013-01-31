;;;; Filters for functions.

(in-package :soundlab.filters)

(defun lowslope (function cutoff dampening)
  "Cut off slopes of FUNCTION greater than CUTOFF by DAMPENING."
  (let ((previous-x 0)
	(previous-y 0))
    (lambda (x)
      (let ((y (funcall function x)))
	(cond ((= 0 x) y)
	      ((= 0 y) 0)
	      ((> (/ (- (abs y) (abs previous-y))
		     (- (abs x) (abs previous-x)))
		  cutoff)
	       (setf previous-x x
		     previous-y (if (= 0 previous-y)
				    (* y dampening)
				    (* previous-y
				       (1+ (* (/ (- y previous-y)
						 previous-y)
					      dampening))))))
	      (t
	       (setf previous-x x
		     previous-y y)))))))
