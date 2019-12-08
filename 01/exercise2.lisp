(ql:quickload "uiop")

(defun get-masses ()
    (mapcar #'parse-integer (uiop:read-file-lines "input")))

(defun fuel-needed (mass)
    (let (
        (fuel-for-mass (- (floor (/ mass 3)) 2)))
        (cond ((<= fuel-for-mass 0) 0)
              (t (+ fuel-for-mass (fuel-needed fuel-for-mass))))))

(defun calculate ()
    (reduce #'+ (mapcar #'fuel-needed (get-masses))))