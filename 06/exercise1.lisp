(ql:quickload '(:uiop :split-sequence))

(defun load-input ()
    (uiop:read-file-lines "input.txt"))

(defun orbit-pairs (input)
    (mapcar
        #'(lambda (orbit-string)
            (let ((split (split-sequence:split-sequence #\) orbit-string)))
                (list (intern (first split)) (intern (second split)))))
        input))

