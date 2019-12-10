(defun int-to-list (val)
    (loop for c across (write-to-string val) collect (digit-char-p c)))

(defun test (digits)
    (reduce
        #'(lambda (a b)
            (let 
                ((prev (first a))
                 (increasing (second a))
                 (already-doubled (third a)))
            (list
                b
                (and (equal t increasing) (<= prev b)) 
                (or (equal t already-doubled) (equal prev b)))))
        digits
        :initial-value '(0 t nil)))

;; (length (collect 147981 691423)) -> 1790
(defun collect (begin end)
    (loop for x from begin to end when
        (let
            ((result (test-2 (int-to-list x))))
        (and (equal t (second result)) (equal t (third result))))
    collect x))