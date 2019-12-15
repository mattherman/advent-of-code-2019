(ql:quickload '(:uiop :split-sequence))

(setf opcode-length 2)
(setf max-mode-length 3)

(defun load-program-memory ()
    (coerce
        (mapcar 
            #'parse-integer 
            (split-sequence:split-sequence #\,
                (first (uiop:read-file-lines "input.txt"))))
        'vector))

(defun int-to-list (val)
    (loop for c across (write-to-string val) collect (digit-char-p c)))

(defun pad-n (x n)
    (if (> n 0) (cons 0 (pad-n x (- n 1))) x))
(defun pad-list-to-n (x n)
    (pad-n x (- n (length x))))

(defun get-at (arr index)
    (aref arr index))
(defun set-at (arr index value)
    (setf (aref arr index) value))

(defun addp (instruction)
    (equal '(0 1) (second instruction)))
(defun multp (instruction)
    (equal '(0 2) (second instruction)))
(defun writep (instruction)
    (equal '(0 3) (second instruction)))
(defun readp (instruction)
    (equal '(0 4) (second instruction)))
(defun haltp (instruction)
    (equal '(9 9) (second instruction)))

(defun instruction-size (instruction)
    (cond ((addp instruction) 4)
          ((multp instruction) 4)
          ((writep instruction) 2)
          ((readp instruction) 2)
          ((haltp instruction) 1)))

(defun positionp (mode)
    (equal 0 mode))
(defun immediatep (mode)
    (equal 1 mode))

;; 1002
;; (1 0 0 2)
;; (0 1 0 0 2)
;; ((0 1 0) (0 2))
(defun parse-opcode (opcode)
    (let ((opcode-list (pad-list-to-n (int-to-list opcode) (+ max-mode-length opcode-length))))
    (list (reverse (subseq opcode-list 0 max-mode-length)) (subseq opcode-list max-mode-length))))
(defun get-instruction (program ip)
    (parse-opcode (get-at program ip)))

(defun get-parameters (program ip instruction)
    (let ((num-parameters (- (instruction-size instruction) 1)))
    (loop for index from (+ ip 1) to (+ ip num-parameters) collect (get-at program index))))
(defun get-parameter-value (program parameter mode)
    (if (positionp mode)
        (get-at program parameter)
        parameter))

(defun execute-add (program instruction parameters)
    (let
        ((input1 (get-parameter-value program (first parameters) (first (first instruction))))
         (input2 (get-parameter-value program (second parameters) (second (first instruction)))))
    (set-at program (third parameters) (+ input1 input2))))

(defun execute-mult (program instruction parameters)
    (let
        ((input1 (get-parameter-value program (first parameters) (first (first instruction))))
         (input2 (get-parameter-value program (second parameters) (second (first instruction)))))
    (set-at program (third parameters) (* input1 input2))))

(defun execute-write (program instruction parameters)
    (set-at program (first parameters) input))

(defun execute-read (program instruction parameters)
    (print (get-at program (first parameters))))

(defun execute-instruction (program instruction parameters)
    (cond ((addp instruction) (execute-add program instruction parameters))
          ((multp instruction) (execute-mult program instruction parameters))
          ((writep instruction) (execute-write program instruction parameters))
          ((readp instruction) (execute-read program instruction parameters))))

(defun execute (program)
    (let ((ip 0))
        (loop
            (setf instruction (get-instruction program ip))
            (if (haltp instruction) (return (get-at program 0)))
            (setf parameters (get-parameters program ip instruction))
            (execute-instruction program instruction parameters)
            (setf ip (+ ip (instruction-size instruction))))))

(setf input 1)

(defun run ()
    (let ((program-memory (load-program-memory)))
    (progn
        (execute program-memory))))