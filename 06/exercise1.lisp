(ql:quickload '(:uiop :split-sequence))

(defparameter *orbits* ())
(defparameter *orbit-counts* ())

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
    (setf *orbits* (orbit-pairs (load-input file)))
    (setf *orbit-counts* ()))

(defun get-orbit (planet)
    (assoc planet *orbits*))

(defun get-orbit-count (planet)
    (cdr (assoc planet *orbit-counts*)))

(defun set-orbit-count (planet count)
    (setf *orbit-counts* (acons planet count *orbit-counts*)) count)

(defun count-orbit (planet)
    (let 
        ((existing-count (get-orbit-count planet))
         (orbit (get-orbit planet)))
    (cond ((not (null existing-count)) existing-count)
          ((not (null orbit)) (set-orbit-count planet (+ 1 (count-orbit (cdr orbit)))))
          (t (set-orbit-count planet 0)))))

(defun count-orbits ()
    (mapcar #'(lambda (pair)
        (count-orbit (car pair)))
    *orbits*))

(defun run ()
    (init "input.txt")
    (reduce #'+ (count-orbits)))