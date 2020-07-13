(defparameter *orbits* ())
(defparameter *planets* ())

(defstruct (planet)
    (name nil)
    (transfers nil)
    (visited nil))

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

(defun create-planets ()
    (mapcar
        #'(lambda (orbit)
            (let*
                (
                 (transfers (cdr orbit))
                 (planet (make-planet :name (car orbit) :transfers transfers)))
            (list planet)))
    *orbits*))

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