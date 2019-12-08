(ql:quickload '(:uiop :split-sequence))

(defun load-program-memory ()
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
    (equal 1 (first instruction)))
(defun multp (instruction)
    (equal 2 (first instruction)))
(defun haltp (instruction)
    (equal 99 (first instruction)))

(defun get-instruction (program ip)
    (let ((instruction (list (get-at program ip))))
    (if (haltp instruction) 
        instruction
        (append 
            instruction
            (list
                (get-at program (+ ip 1))
                (get-at program (+ ip 2))
                (get-at program (+ ip 3)))))))

(defun execute-instruction (program instruction)
    (let
        ((input1 (get-at program (second instruction)))
         (input2 (get-at program (third instruction))))
            (cond ((addp  instruction)
                    (set-at program (fourth instruction) (+ input1 input2)))
                  ((multp instruction)
                    (set-at program (fourth instruction) (* input1 input2))))))

(defun execute (program)
    (let ((ip 0))
        (loop
            (setf instruction (get-instruction program ip))
            (if (haltp instruction) (return (get-at program 0)))
            (execute-instruction program instruction)
            (setf ip (+ ip 4)))))

;; (run 12 2) -> 7210630
(defun run (noun verb)
    (let ((program-memory (load-program-memory)))
    (progn
        (set-at program-memory 1 noun)
        (set-at program-memory 2 verb)
        (execute program-memory))))

(defun find-noun-verb (expected-output)
    (loop named outer for x from 1 to 99 do
        (loop for y from 1 to 99 do
            (if (equal expected-output (run x y))
                (return-from outer (list x y))))))
