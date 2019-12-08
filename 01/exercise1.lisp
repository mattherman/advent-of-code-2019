(ql:quickload "uiop")

(defun get-masses ()
    (mapcar #'parse-integer (uiop:read-file-lines "input")))

(defun fuel-needed (mass)
    (- (floor (/ mass 3)) 2))

(defun calculate ()
    (reduce #'+ (mapcar #'fuel-needed (get-masses))))