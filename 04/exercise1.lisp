(defun int-to-list (val)
    (loop for c across (write-to-string val) collect (digit-char-p c)))

(defun increasing (digits)
    (apply #'<= digits))

;; Example - (1 2 2 3)
;; ()
;; ((1))
;; ((2) (1))
;; ((2 2) (1))
;; ((3) (2 2) (1))
;;
;; acc = ((1)) digit = 1
;; (list (cons 1 (car ((1)) )) (cadr ((1)) ))
;; (list (cons 1 (1) )         nil)
;; (list (1 1)                 nil)
;; ((1 1) nil)
;;
;; acc = ((1)) digit = 2
;; (cons (list 2) ((1)) )
;; (cons (2)      ((1)) )
;; ((2) (1))
(defun split-sequences (digits)
    (reduce
        #'(lambda (acc digit)
            (if (equal (car (car acc)) digit)
                (cons (cons digit (car acc)) (cdr acc))
                (cons (list digit) acc)))
        digits
        :initial-value '()))

(defun any (fn items)
    (not
        (equal nil (find-if (lambda (x) (funcall fn x)) items))))

(defun includes-sequence-of-n (digits n)
    (any #'(lambda (x) (>= (length x) n)) (split-sequences digits)))

(defun test (password)
    (let ((digits (int-to-list password)))
    (and (increasing digits) (includes-sequence-of-n digits 2))))

(defun find-matching-passwords (begin end)
    (loop for password from begin to end when (test password) collect password))

(defun run ()
    (length (find-matching-passwords 147981 691423)))