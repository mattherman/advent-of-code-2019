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
    (equal '(0 1) (first instruction)))
(defun multp (instruction)
    (equal '(0 2) (first instruction)))
(defun writep (instruction)
    (equal '(0 3) (first instruction)))
(defun readp (instruction)
    (equal '(0 4) (first instruction)))
(defun haltp (instruction)
    (equal '(9 9) (first instruction)))

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

;; ip + n = 1002, 4, 3, 4
;; num-parameters = 3
;; (4 3 4)
(defun get-parameters (program ip opcode)
    (let ((num-parameters (- (instruction-size (list opcode)) 1)))
    (loop for index from (+ ip 1) to (+ ip num-parameters) collect (get-at program index))))

;; ip + n = 1002, 4, 3, 4
;; opcode = ((0 1 0) (0 2))
;; parameters = (4 3 4)
;; ((0 2) (0 4) (1 3) (0 4))
(defun get-instruction (program ip)
    (let* 
        ((opcode-and-modes (parse-opcode (get-at program ip)))
         (opcode (second opcode-and-modes))
         (modes (first opcode-and-modes)))
    (cons
        opcode
        (mapcar #'list modes (get-parameters program ip opcode)))))

;; (0 10) -> value at position 10 in memory
;; (1 10) -> 10
(defun get-parameter-value-by-mode (program parameter)
    (let ((mode (first parameter)) (value (second parameter)))
    (if (positionp mode)
        (get-at program value)
        value)))

;; Sets are always performed by position so ignore mode
(defun set-parameter-value-by-mode (program parameter value)
    (set-at program (second parameter) value))

(defun execute-add (program instruction)
    (let
        ((input1 (get-parameter-value-by-mode program (second instruction)))
         (input2 (get-parameter-value-by-mode program (third instruction))))
    (set-parameter-value-by-mode program (fourth instruction) (+ input1 input2))))

(defun execute-mult (program instruction)
    (let
        ((input1 (get-parameter-value-by-mode program (second instruction)))
         (input2 (get-parameter-value-by-mode program (third instruction))))
    (set-parameter-value-by-mode program (fourth instruction) (* input1 input2))))

(defun execute-write (program instruction)
    (let ((parameter (second instruction)))
    (set-parameter-value-by-mode program parameter input)))

(defun execute-read (program instruction)
    (let ((parameter (second instruction)))
    (print (get-parameter-value-by-mode program parameter))))

(defun execute-instruction (program instruction)
    (cond ((addp instruction) (execute-add program instruction))
          ((multp instruction) (execute-mult program instruction))
          ((writep instruction) (execute-write program instruction))
          ((readp instruction) (execute-read program instruction))))

(defun execute (program)
    (let ((ip 0))
        (loop
            (setf instruction (get-instruction program ip))
            (if (haltp instruction) (return 'HALT))
            (execute-instruction program instruction)
            (setf ip (+ ip (instruction-size instruction))))))

(setf input 1)

;; With input "1", expect 4511442
(defun run ()
    (execute (load-program-memory)))