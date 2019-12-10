(defun int-to-list (val)
    (loop for c across (write-to-string val) collect (digit-char-p c)))

(defun increasing (digits)
    (apply #'<= digits))

(defun split-sequences (digits)
    (reduce
        #'(lambda (acc digit)
            (if (equal (car (car acc)) digit)
                (list (cons digit (car acc)) (cadr acc))
                (cons (list digit) acc)))
        digits
        :initial-value '()))

(defun any (fn items)
    (not
        (equal nil (find-if (lambda (x) (funcall fn x)) items))))

(defun includes-sequence (digits)
    (any #'(lambda (x) (>= (length x) 2)) (split-sequences digits)))

(defun test (digits)
    (and (increasing digits) (includes-sequence digits)))

(defun find-matching-passwords (begin end)
    (loop for x from begin to end when (test (int-to-list x)) collect x))

;; Expect 1790
(defun run ()
    (length (find-matching-passwords 147981 691423)))