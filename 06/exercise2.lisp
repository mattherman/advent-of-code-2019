(defparameter *orbits* ())

(defun load-input (file)
    (uiop:read-file-lines file))

;; Flips the input pair of "A)B" to be (B . A) to
;; indicate that B orbits A in the association list
(defun intern-pair (vals)
    (cons (intern (second vals)) (intern (first vals))))

(defun orbit-pairs (input)
    (mapcar
        #'(lambda (orbit-string)
            (intern-pair
                (split-sequence:split-sequence #\) orbit-string)))
        input))

(defun init (file)
    (setf *orbits* (orbit-pairs (load-input file))))

(defun get-orbit (planet)
    (assoc planet *orbits*))

(defun get-orbits (planet)
    (let ((orbit (get-orbit planet)))
    (cond ((null orbit) (list planet))
          (t (cons planet (get-orbits (cdr orbit)))))))

(defun run ()
    (init "input.txt")
    (let
        ((you-orbits (get-orbits 'YOU))
         (san-orbits (get-orbits 'SAN)))
    (car (reverse (intersection you-orbits san-orbits)))))