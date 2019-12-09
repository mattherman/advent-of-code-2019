(ql:quickload '(:uiop :split-sequence))

(defun load-path-data ()
    (mapcar
        #'(lambda (line) (split-sequence:split-sequence #\, line))
        (uiop:read-file-lines "input.txt")))

(defun convert-string-to-vector (input)
    (let 
        ((direction (subseq input 0 1))
         (magnitude (subseq input 1 (length input))))
            (list
                (cond ((equal "R" direction) 'right)
                      ((equal "L" direction) 'left)
                      ((equal "U" direction) 'up)
                      ((equal "D" direction) 'down))
                (parse-integer magnitude))))

(defun convert-strings-to-vectors (path-strings)
    (mapcar #'convert-string-to-vector path-strings))

(defun rightp (vector)
    (equal 'right (first vector)))
(defun leftp (vector)
    (equal 'left (first vector)))
(defun upp (vector)
    (equal 'up (first vector)))
(defun downp (vector)
    (equal 'down (first vector)))
(defun magnitude (vector)
    (second vector))
(defun vector-equal (v1 v2)
    (and
        (equal (first v1) (first v2))
        (equal (second v1) (second v2))))

(defun walk-right (coord)
    (list (+ (first coord) 1) (second coord)))
(defun walk-left (coord)
    (list (- (first coord) 1) (second coord)))
(defun walk-up (coord)
    (list (first coord) (+ (second coord) 1)))
(defun walk-down (coord)
    (list (first coord) (- (second coord) 1)))

(defun walk (coord walkfn n)
    (if (not (equal 0 n))
        (let ((new-coord (funcall walkfn coord)))
        (cons
            new-coord
            (walk new-coord walkfn (- n 1))))))

(defun get-walker (vector)
    (cond ((rightp vector) #'walk-right)
          ((leftp vector)  #'walk-left)
          ((upp vector)    #'walk-up)
          ((downp vector)  #'walk-down)))

(defun collect-coords (starting-coord vector)
    (walk
        starting-coord
        (get-walker vector)
        (magnitude vector)))

(defun last-element (x)
    (first (last x)))

(defun get-coordinates-visited (path)
    (reduce
        #'(lambda (coords vector)
            (let ((current-coord (last-element coords)))
            (append coords (collect-coords current-coord vector))))
        path
        :initial-value '((0 0))))

(defun manhattan-distance (coord)
    (+ (abs (first coord)) (abs (second coord))))

;; Expected answer 1211
(defun run ()
    (progn
        (setf path-data (load-path-data))
        (setf first-path-vectors (convert-strings-to-vectors (first path-data)))
        (setf second-path-vectors (convert-strings-to-vectors (second path-data)))
        (apply #'min
            (remove-if #'(lambda (x) (equal 0 x))
                (mapcar
                    #'manhattan-distance 
                    (intersection 
                        (get-coordinates-visited first-path-vectors)
                        (get-coordinates-visited second-path-vectors)
                        :test #'vector-equal))))))
