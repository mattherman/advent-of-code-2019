(ql:quickload '(:uiop :split-sequence))

(defun get-instructions ()
    (coerce
        (mapcar 
            #'parse-integer 
            (split-sequence:split-sequence #\,
                (first (uiop:read-file-lines "input.txt"))))
        'vector))

(defun get-at (arr index)
    (aref arr index))
(defun set-at (arr index value)
    (setf (aref arr index) value))

(defun addp (instruction)
    (equal 'add (first instruction)))
(defun multp (instruction)
    (equal 'mult (first instruction)))
(defun haltp (instruction)
    (equal 'halt (first instruction)))

(defun value-to-intcode (intcode)
    (cond ((equal 1 intcode)  'add)
          ((equal 2 intcode)  'mult)
          ((equal 99 intcode) 'halt)))

(defun get-instruction (program pc)
    (let ((instruction (list (value-to-intcode (get-at program pc)))))
    (if (haltp instruction) 
        instruction
        (append 
            instruction
            (list
                (get-at program (+ pc 1))
                (get-at program (+ pc 2))
                (get-at program (+ pc 3)))))))

(defun execute-instruction (program instruction)
    (let
        ((input1 (get-at program (second instruction)))
         (input2 (get-at program (third instruction))))
            (cond ((addp  instruction)
                    (set-at program (fourth instruction) (+ input1 input2)))
                  ((multp instruction)
                    (set-at program (fourth instruction) (* input1 input2))))))

(defun execute (program)
    (let ((pc 0))
        (loop
            (setf instruction (get-instruction program pc))
            (if (haltp instruction) (return program))
            (execute-instruction program instruction)
            (setf pc (+ pc 4)))))

;; The problem says that you should replace
;; position 1 with the value 12 and position 2
;; with the value 2 before running the program
(setf instructions (get-instructions))
(set-at instructions 1 12)
(set-at instructions 2 2)

;; (execute instructions)
;; Position 0 after execution will be 7210630