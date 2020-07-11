(ql:quickload '(:uiop :split-sequence))

(defun load-input (file)
    (uiop:read-file-lines file))

(defun orbit-pairs (input)
    (mapcar
        #'(lambda (orbit-string)
            (let ((split (split-sequence:split-sequence #\) orbit-string)))
                (cons (intern (first split)) (intern (second split)))))
        input))

(defparameter *orbits* (orbit-pairs (load-input "input.txt")))
(defparameter *orbit-counts* ())

(defun get-orbit (planet)
    (assoc planet *orbits*))

(defun get-orbit-count (planet)
    (assoc planet *orbit-counts*))

(defun set-orbit-count (planet count)
    (acons planet count *orbit-counts*))

(defun count-orbit (planet)
    (let ((existing-count (get-orbit-count planet)))
    (cond ((null existing-count)
            (let ((orbit (get-orbit planet)))
            (cond ((null orbit) (set-orbit-count planet 0))
                  (t (set-orbit-count planet (+ 1 (count-orbit (cdr orbit))))))))
          (t existing-count))))