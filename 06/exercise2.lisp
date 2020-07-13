(defparameter *orbits* ())
(defparameter *planets* ())

(defstruct (planet)
    (name nil)
    (transfers nil)
    (visited nil))

(defun load-input (file)
    (uiop:read-file-lines file))

;; Takes input of "A)B" and creates (A . B) since we will
;; be traversing from the root of the tree down
(defun intern-pair (vals)
    (cons (intern (first vals)) (intern (second vals))))

(defun orbit-pairs (input)
    (mapcar
        #'(lambda (orbit-string)
            (intern-pair
                (split-sequence:split-sequence #\) orbit-string)))
        input))

(defun create-planets (orbits)
    ())

(defun traverse-transfers (planet-orbited planet)
    ())

(defun init (file)
    (setf *orbits* (orbit-pairs (load-input file))))

(defun get-orbit (planet)
    (assoc planet *orbits*))

(defun get-orbit-of (planet))

(defun get-orbits (planet)
    (let ((orbit (get-orbit planet)))
    (cond ((null orbit) (list planet))
          (t (cons planet (get-orbits (cdr orbit)))))))

(defun find-shortest-path (from to))

(defun run ()
    (init "input.txt")
    (let
        ((you-orbits (get-orbits 'YOU))
         (san-orbits (get-orbits 'SAN)))
    (car (reverse (intersection you-orbits san-orbits)))))