(defun range (from &optional to (step 1))
  (cond ((null to) (range 0 from 1))
        ((<= (* (- to from) step) 0) nil) ;; check if step moves in right direction
        ((equal from to) nil)
        (t (cons from (range (+ from step) to step)))))

(defun dispatch (&rest funcs)
  #'(lambda (&rest args)
      (if (null funcs) nil
          ;; else
          (let ((result (apply (car funcs) args)))
            (if result result
                ;; else
                (apply (apply #'dispatch (cdr funcs)) args))))))


(defun parse-float (strg)
  (if (stringp strg)
      (with-input-from-string (s strg)
        (let ((res (read s)))
          (if (eq (type-of res) 'single-float) res nil)))))


(defun parse-int (strg)
  (ignore-errors (parse-integer strg)))


(defun wrap-fn (f pre &optional (post #'(lambda (arg) arg)))
  #'(lambda (&rest args)
      (cond ((equal args '(:orig)) f)
            (t (funcall post (apply f (apply pre args)))))))