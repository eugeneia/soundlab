;;;; Make signals duck each other.

(in-package :soundlab.combinators)

(defun gate (sidechain response)
  "Return a gate signal triggered by SIDECHAIN with RESPONSE."
  (let ((in 0)
        (release 0)
        (xd 0))
    (lambda (x)
      (let ((y (abs (funcall sidechain x))))
        (if (>= y in)
            (setf in y
                  release response)
            (decf release (- x xd)))
        (let ((d (* in (/ release response))))
          (setf in d
                xd x)
          (- 1 d))))))
                   

(defun duck-2 (sidechain signal response)
  "Add two signals while SIGNAL will make room for SIDECHAIN with
RESPONSE."
  (add (multiply signal (gate sidechain response))
       sidechain))

(defun duck (response &rest signals)
  "Add SIGNALS while ducking each other from left to right with
RESPONSE."
  (reduce (lambda (x y)
            (duck-2 x y response))
          signals))
