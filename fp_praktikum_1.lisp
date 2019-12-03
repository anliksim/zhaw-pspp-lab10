#!/usr/local/bin/sbcl --script

;; loads common utils
(load "util.lisp")

;;
;; Functions
;;
(load "base.lisp")

(defmacro setfun (symb fun)
    `(prog1 ',symb (setf (symbol-function ',symb) ,fun)))


(defun repeat (times value)
  (mapcar (lambda (n) value)
          (range times)))


(defun repeatedly (times fun)
  (mapcar fun (range times)))


(setfun num (dispatch #'parse-int #'parse-float #'identity))


(defun num-args (&rest args)
  (mapcar #'num args))



(defun partial (f &rest args)
  (lambda (&rest more-args)
    (apply f (append args more-args))))


(defun factorial (n &optional (fact 1))
  (if (<= n 1) fact
      (factorial (- n 1) (* n fact))))


(defun trampoline (fun &rest args)
  (let ((result (apply fun args)))
    (loop while (functionp result) do
      (setq result (funcall result)))
    result))


;;
;; Exercise
;;


(pex (repeatedly 5 (lambda (n) 'hello)))
(pex (repeatedly 5 (lambda (n) (+ 1 (random 5)))))
(pex (repeatedly 5 (lambda (n) (concatenate 'string "id" (write-to-string n)))))

;; Wird hier eine Closure verwendet? Ja
;; Ist always eine Funktion höherer Ordnung? Ja
;;
(defun always (val) (lambda (&rest r) val))

(pex (repeatedly 5 (always 'hello)))


;; Was macht die Funktion num? Parsen von einem string zu einem int oder float
;; Welchen Zweck erfüllt die Funktion identity in der Definition von num? Rückgabe der Eingabe
;;
(pex (num nil))
(pex (num "42"))
(pex (num "0.42"))
(pex (num "string"))

(setfun add (wrap-fn #'+ #'num-args))

(pex (add "40" "2"))
(pex (add "0.4" "0.02")) ; nicht genau
(pex (add "1" "2" "3"))
; (pex (add "a" "b"))


;; Welche Kriterien muss eine Funktion erfüllen, um endrekursiv(tail recursive) zu sein?
;; Der Rekursionsaufruf muss das letze Statement sein (darum tail).
;;
;; Wozu dient hier das zweite Argument? Result propagation
;;
(defun partial-factorial (n &optional (fact 1))
  (if (<= n 1) (lambda (&rest args) fact)
      (partial 'partial-factorial (- n 1) (* n fact))))

(pex (partial-factorial 3)) ; closure
(pex (funcall (partial-factorial 3))) ; closure
(pex (funcall (funcall (partial-factorial 3)))) ; closure
(pex (funcall (funcall (funcall (partial-factorial 3))))) ; result

(pex (trampoline (partial-factorial 3))) ; result
