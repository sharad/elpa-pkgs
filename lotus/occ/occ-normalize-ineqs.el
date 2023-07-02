;;; occ-normalize-ineqs.el --- normalize in-equalities  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sharad

;; Author: sharad <>
;; Keywords: convenience, abbrev

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(provide 'occ-normalize-ineqs)


(require 'occ-obj)
(require 'occ-prop-intf)


(defvar occ-ineqs '())
(defvar occ-normalized-ineq '())

(cl-defgeneric occ-add-ineq (operator
                             prop1
                             prop2)
  "Add relative property.")

(cl-defmethod occ-add-ineq ((operator symbol)
                            (prop1    symbol)
                            (prop2    symbol))
  "Add relative property."
  (cl-assert (memq operator '(gt lt eq)))
  (cl-pushnew (list operator prop1 prop2)
              occ-ineqs))


(defvar occ-ineqs '())

(defun occ-obj-propetirs-for-rank ()
  (delete-dups (append (occ-obj-properties-to-calculate-rank 'occ-tsk)
                       (occ-obj-properties-to-calculate-rank 'occ-obj-ctx-tsk))))

(defun occ-do-assert-sexp-ineq (ineq)
  (occ-assert (= 3 (length ineq)))
  (occ-assert (memq (car ineq) '(> < =)))
  (occ-assert (not (cl-every #'consp (cdr ineq))))
  (occ-assert (cl-some #'symbolp (cdr ineq)))
  (dolist (el (cdr ineq))
    (cond ((symbolp el)
           (occ-assert (symbolp el)))
          ((consp el)
           (progn
             (occ-assert (= 3 (length el)))
             (occ-assert (memq (car el) '(* +)))
             (occ-assert (or (and (symbolp (car el))
                                  (numberp (cadr el)))
                             (and (numberp (car el))
                                  (symbolp (cadr el))))))))))
(defun occ-do-assert-math-ineq (ineq)
  "Assert variables of INEQ inequality are from properties."
  (cond ((and (listp ineq)
              (eql 'var (car ineq)))
         (let ((var (cadr ineq))
               (properties (occ-obj-propetirs-for-rank)))
           (occ-assert (memq var properties) nil "variable %s is not in properties %s" var properties)))
        ((atom ineq) ineq)
        ((listp ineq)
         (dolist (ine ineq)
           (occ-do-assert-math-ineq ine))))
  t)
(defun occ-do-assert-math-ineq-has-prop-p (ineq property)
  "Check if PROPERTY variable is present in INEQ inequality"
  (cond ((and (listp ineq)
              (eql 'var (car ineq)))
         (let ((var (cadr ineq)))
           (eql property var)))
        ((atom ineq) nil)
        ((listp ineq)
         (cl-some #'(lambda (e) (occ-do-assert-math-ineq-has-prop-p e property)) ineq))))

;; (occ-do-assert-math-ineq-has-prop (occ-obj-ineq-wash (occ-obj-math-read-expr "root + nil") 'key) 'key)

(defun occ-math-read-sexp-expr (sexp)
  "Converts a Lisp sexp expression SEXP into an equivalent expression."
  (cond ((atom sexp) (cond ((assoc (prin1-to-string sexp) math-standard-opers) (cadr (assoc (symbol-name sexp) math-standard-opers)))
                           ((symbolp sexp) `(var ,sexp ,(intern (concat "var-" (symbol-name sexp)))))
                           (t sexp)))
        ((listp sexp) (cons (occ-math-read-sexp-expr (car sexp))
                            (mapcar #'occ-math-read-sexp-expr (cdr sexp))))
        (t sexp)))
(defun occ-obj-math-read-expr (ineq)
  "Return normalized calc math expression."
  (calc-normalize (cond ((stringp ineq) (math-read-expr ineq))
                        ((consp ineq)   (occ-math-read-sexp-expr ineq))
                        (t (occ-error "ineq %s not string or list" ineq)))))
(defun occ-obj-ineq-wash (ineq property)
  (cond ((and (listp ineq)
              (eql 'var (car ineq))
              (eql 'nil (cadr ineq)))
         (list 'var property (intern (concat "var-" (symbol-name property)))))
        ((atom ineq) ineq)
        ((listp ineq) (cons (occ-obj-ineq-wash (car ineq) property)
                            (mapcar #'(lambda (ineq) (occ-obj-ineq-wash ineq property)) (cdr ineq))))
        (t ineq)))
;; (occ-obj-ineq-wash (occ-obj-math-read-expr "a + nil") 'xx)
;; (occ-do-assert-ineq (occ-obj-ineq-wash (occ-obj-math-read-expr "root + nil") 'key))
(defun occ-obj-topo-sort-ineqs-expr (inequalities)
  (let ((graph     '())
        (in-degree '()))
    (dolist (ineq inequalities)
      (cl-destructuring-bind (op var1 var2) ineq
        (unless (member* var1 graph :key #'car)
          (push (list var1) graph)
          (push (cons var1 0) in-degree))
        (unless (member* var2 graph :key #'car)
          (push (list var2) graph)
          (push (cons var2 0) in-degree))
        (cond ((eq op 'calcFunc-gt)
               (progn
                 (cl-pushnew (cons var2 'gt) (cdr (assoc var1 graph)))
                 (cl-incf (cdr (assoc var2 in-degree)))))
              ((eq op 'calcFunc-lt)
               (progn
                 (cl-pushnew (cons var1 'lt) (cdr (assoc var2 graph)))
                 (cl-incf (cdr (assoc var1 in-degree)))))
              ((eq op 'calcFunc-eq)
               (progn
                 (cl-pushnew (cons var1 'eq) (cdr (assoc var2 graph)))
                 (cl-incf (cdr (assoc var1 in-degree))))))))
    (let ((in-degree-old (mapcar #'copy-list in-degree))
          (sorted-vars   '())
          (queue (mapcar #'(lambda (x) (cons x 'gt))
                         (cl-remove-if-not #'(lambda (x)
                                               (= 0 (cdr (assoc x in-degree))))
                                           (mapcar #'car graph))))
          (var nil))
       (while (setf var (pop queue))
         (setq sorted-vars (append sorted-vars (list var)))
         (dolist (vertex (cdr (assoc (car var) graph)))
           (cl-decf (cdr (assoc (car vertex) in-degree)))
           (if (= 0 (cdr (assoc (car vertex) in-degree)))
               (setf queue (append queue (list vertex))))))
       (list (list :inequalities inequalities)
             (list :graph graph)
             (list :in-degree-old in-degree-old)
             (list :in-degree in-degree)
             (list :sorted-vars sorted-vars)))))
(defun occ-obj-gen-constant (op)
  (let* ((const-var (gensym (concat "cx" "-")))
         (calc-const-var `(var ,const-var ,(intern (concat "var-" (symbol-name const-var))))))
    (if (memq op '(calcFunc-gt calcFunc-lt))
        `(+ 1 ,calc-const-var)
      calc-const-var)))
(defun occ-obj-ineq2eq (ineq)
  (cond ((and (consp ineq)
              (symbolp (car ineq))
              (memq (car ineq) '(calcFunc-gt calcFunc-geq calcFunc-lt calcFunc-leq)))
         (occ-assert (= 3 (length ineq)))
         (let ((arg1 (cadr ineq))
               (arg2 (caddr ineq)))
           (let ((const-expr (occ-obj-gen-constant (car ineq))))
             (cond ((memq (car ineq) '(calcFunc-gt calcFunc-geq))
                    `(calcFunc-eq ,arg1 (+ ,arg2 ,const-expr)))
                   ((memq (car ineq) '(calcFunc-lt calcFunc-leq))
                    `(calcFunc-eq ,arg2 (+ ,arg1 ,const-expr)))))))
        ((listp ineq) (cons (occ-obj-ineq2eq (car ineq))
                            (mapcar #'occ-obj-ineq2eq (cdr ineq))))
        (t ineq)))
(defun occ-normalize-ineqs (ineqs)
  ;; (let ((ineqs (mapcar #'caddr ineqs)))
  ;;   (occ-obj-order-ineqs-expr ineqs))
  (let ((eqs  (mapcar #'(lambda (e) (occ-obj-ineq2eq (cdr e)))
                      ineqs))
        (vars (mapcar #'(lambda (v) (let ((var (car v)))
                                      `(var ,var ,(intern (concat "var-" (symbol-name var))))))
                      ineqs)))
    (calc-normalize `(calcFunc-solve (vec ,@eqs)
                                     (vec ,@vars)))))

(defun occ-find-vars (eq vars)
  (cond ((and (listp eq)
              (eql 'var (car eq)))
         (list eq (append (list (cadr eq)) vars)))
        ((atom eq) (list eq vars))
        ((listp eq)
         (let ((first-el (occ-find-vars (car eq) vars))
               (rest-els (mapcar #'(lambda (e) (occ-find-vars e vars)) (cdr eq))))
           (list (cons (car first-el) (mapcar #'car rest-els))
                 (append (cadr first-el)
                         (apply #'append  (mapcar #'cadr rest-els))))))
        (t (list eq vars))))

(defun occ-eqs-exprs (eqs)
  (mapcar #'caddr (cdr eqs)))

(defun occ-find-expr-vars (expr)
  (cond ((and (listp expr)
              (eql 'var (car expr)))
         (list (cadr expr)))
        ((atom expr) nil)
        ((listp expr)
         (let ((first-el (occ-find-expr-vars (car expr)))
               (rest-els (mapcar #'occ-find-expr-vars (cdr expr))))
           (append first-el
                   (apply #'append  rest-els))))
        (t nil)))

(defun occ-find-exprs-vars (exprs)
  (delete nil (delete-dups (apply #'append (mapcar #'occ-find-expr-vars exprs)))))


(defun occ-find-eqs-vars (eqs)
  (occ-find-exprs-vars (occ-eqs-exprs eqs)))



(defun occ-do-add-ineq-1 (ineq property)
  (let ((ineq (occ-obj-ineq-wash (occ-obj-math-read-expr ineq) property)))
    (when (and (occ-do-assert-math-ineq-has-prop-p ineq property)
               (occ-do-assert-math-ineq ineq))
     (if (occ-normalize-ineqs (append occ-ineqs `(,(cons property ineq))))
         (if (assoc property occ-ineqs)
             (setcdr (assoc property occ-ineqs) ineq)
           (cl-pushnew (cons property ineq) occ-ineqs))))))

(defun occ-obj-ineq-1 (property)
  (cdr (assoc property occ-ineqs)))


(occ-do-add-ineq-1 "root > (key + 2)" 'root)
(occ-do-add-ineq-1 "key > 10" 'key)
(occ-do-add-ineq-1 "status  > 2 * root" 'status)

(occ-obj-ineq-wash (occ-obj-math-read-expr "root > (key + 2)") 'root)

;; (math-format-flat-expr (occ-normalize-ineqs occ-ineqs) 0)




(occ-find-expr-vars (cadr (occ-eqs-exprs (occ-normalize-ineqs occ-ineqs))))
(occ-find-exprs-vars (occ-get-eqs (occ-normalize-ineqs occ-ineqs)))
(occ-find-eqs-vars (occ-normalize-ineqs occ-ineqs))

(occ-obj-propetirs-for-rank)

(occ-obj-ineq-wash (occ-obj-math-read-expr "2 * root + 1  > root +  (key + 2)") 'root)
(occ-obj-ineq2eq (calc-normalize (math-read-expr "x > b +2")))

(equal (math-read-expr "x = (a + 2)")
       (occ-math-read-sexp-expr '(= x (+ a 2))))

(calc-normalize (list 'vec (car (math-read-exprs " a + b > (a + c)"))))


(setq inequalities '((< a b) (> c b) (< c d)))
(setq sorted-variables (order-variables inequalities))
(order-variables inequalities)

(defun order-variables (inequalities)
  (let ((graph     '())
        (in-degree '()))
    (dolist (ineq inequalities)
      (cl-destructuring-bind (op var1 var2) ineq
        (unless (member* var1 graph :key #'car)
          (push (list var1) graph)
          (push (cons var1 0) in-degree))
        (unless (member* var2 graph :key #'car)
          (push (list var2) graph)
          (push (cons var2 0) in-degree))
        (if (eq op '<)
            (progn
              (cl-pushnew var2 (cdr (assoc var1 graph)))
              (cl-incf (cdr (assoc var2 in-degree))))
          (progn
            (cl-pushnew var1 (cdr (assoc var2 graph)))
            (cl-incf (cdr (assoc var1 in-degree)))))))
    (let ((in-degree-old (mapcar #'copy-list in-degree))
          (sorted-vars   '())
          (queue (cl-remove-if-not #'(lambda (x)
                                       (= 0 (cdr (assoc x in-degree))))
                                   (mapcar #'car graph)))
          (var nil))
       (while (setf var (pop queue))
         (setq sorted-vars (append sorted-vars (list var)))
         (dolist (vertex (cdr (assoc var graph)))
           (cl-decf (cdr (assoc vertex in-degree)))
           (if (= 0 (cdr (assoc vertex in-degree)))
               (setf queue (append queue (list vertex))))))
       (list (list :inequalities inequalities)
             (list :graph graph)
             (list :in-degree-old in-degree-old)
             (list :in-degree in-degree)
             (list :sorted-vars sorted-vars)))))


(setq inequalities '((< a c) (> b c) (< b d) (= a z) (= x d)))
(setq sorted-variables (order-variables inequalities))
(order-variables '((< a c) (> b c) (< b d) (= a z) (= x d)))
(z a c b d x)
(order-variables '((< (+ a 10) c) (> b (* 2 c)) (< b d) (= a z) (= x d)))
(z (* 2 c) (+ a 10) a b c d x)

(order-variables '((< (+ a 10) c) (> b (* 2 c)) (< b d) (= a z) (= x d)))
((:inequalities ((< (+ a 10) c) (> b (* 2 c)) (< b d) (= a z) (= x d)))
 (:graph ((x) (z a) (a) (d x) ((* 2 c) b) (b d) (c) ((+ a 10) c)))
 (:in-degree-old ((x . 1) (z . 0) (a . 1) (d . 1) ((* 2 c) . 0) (b . 1) (c . 1) ((+ a 10) . 0)))
 (:in-degree ((x . 0) (z . 0) (a . 0) (d . 0) ((* 2 c) . 0) (b . 0) (c . 0) ((+ a 10) . 0)))
 (:sorted-vars (z (* 2 c) (+ a 10) a b c d x)))

(order-variables '((< a c) (> b c) (< b d) (= a z) (= x d)))

((:inequalities ((< a c) (> b c) (< b d) (= a z) (= x d)))
 (:graph ((x) (z a) (d x) (b d) (c b) (a c)))
 (:in-degree-old ((x . 1) (z . 0) (d . 1) (b . 1) (c . 1) (a . 1)))
 (:in-degree ((x . 0) (z . 0) (d . 0) (b . 0) (c . 0) (a . 0)))
 (:sorted-vars (z a c b d x)))


(defun occ-get-expr-array (ineqs)
  (cadr (assoc :sorted-vars (order-variables-topo-sort ineqs))))

(setq new-set '())
(pushnew 'a new-set)



(defun reduce-left (expr))


(calc-normalize '(calcFunc-solve (vec (calcFunc-eq (* 2 (var a var-a)) (var d var-d)) (calcFunc-eq (+ (var b var-b) (var c1 var-c1)) (var d var-d)) (calcFunc-eq (+ (+ (var a var-a) (var c1 var-c1)) (var c2 var-c2)) (var d var-d)))
                                 (vec (var a var-a) (var b var-b))))

(math-simplify '(vec (calcFunc-eq (* 2 (var a var-a)) (var d var-d)) (calcFunc-eq (+ (var b var-b) (var c1 var-c1)) (var d var-d)) (calcFunc-eq (+ (+ (var a var-a) (var c1 var-c1)) (var c2 var-c2)) (var d var-d))))

(calc-normalize '(calcFunc-solve (vec (calcFunc-eq (+ (var a var-a) (var b var-b)) (var d var-d)) (calcFunc-eq (+ (var b var-b) (var c var-c)) (var z var-z)) (calcFunc-eq (+ (+ (var a var-a) (var c var-c)) 10) (var z var-z)))
                                 (vec (var a var-a) (var b var-b) (var c var-c))))

(vec (calcFunc-eq (var a var-a) (- (+ (- (var d var-d) (var z var-z)) (/ (- (* 2 (var z var-z)) (var d var-d)) 2)) 5))
     (calcFunc-eq (var b var-b) (+ (+ (var z var-z) (/ (- (var d var-d) (* 2 (var z var-z))) 2)) 5))
     (calcFunc-eq (var c var-c) (- (/ (- (* 2 (var z var-z)) (var d var-d)) 2) 5)))


(math-read-exprs "[ a + b = 1]")
(math-read-exprs "solve([x+y=3, x-y=1], [x,y])")
(math-format-flat-expr '(vec (calcFunc-eq (var a var-a) (float 55 -1)) (calcFunc-eq (var b var-b) (float 45 -1))) 0)
(math-format-flat-expr (car (math-read-exprs "[ a + b = 1]")) 0)

(calc-normalize "[a + b = c1 , a - b = c2]")
(calc-normalize "a - b > 0")
(calc-eval "solve([a - b > 0], [a,b])")
(calc-eval "solve([a + b = c1 , a - b = c2], [a,b])")



(setq xxx '((a)))

(pushnew 'c (cdr (cl-assoc 'a xxx)))


;; (length '(a () x))

;; (occ-obj-properties-to-calculate-rank 'occ-tsk)
;; (occ-obj-properties-to-calculate-rank 'occ-obj-ctx-tsk)

(defvar occ-tsk-normalized-ineq '(none 1 key 10 status 20 current-clock 40 timebeing 50 root 10 currfile 20))
(defvar occ-obj-ctx-tsk-normalized-ineq '(none 1 root 10 currfile 20))


(defun occ-ineq-sum ()
  (cl-reduce #'+
             (mapcar #'cdr
                     (cl--plist-to-alist occ-tsk-normalized-ineq))))

(defun occ-ineq-get (prop value)
  (let ((factor (plist-get occ-tsk-normalized-ineq
                           prop)))
    (* factor
       value)))


(cl-defgeneric occ-obj-ineq-rankprop (obj prop)
  "Get normalized rank.")

(cl-defmethod occ-obj-ineq-rankprop ((obj occ-tsk)
                                     (prop symbol))
  (occ-ineq-get prop
                (occ-obj-rankprop obj prop)))

(cl-defmethod occ-obj-ineq-rankprop ((obj  occ-obj-ctx-tsk)
                                     (prop symbol))
  (occ-ineq-get prop
                (occ-obj-rankprop obj prop)))



(defvar occ-ineqs-collection '((key (gt (* 10 none)))))
(defvar occ-ineq-in-resolve-collection)
(defun occ-normalize (collection) (ignore collection))



(defvar occ-ineqs '())
(defvar occ-normalized-ineq '())

(cl-defgeneric occ-add-ineq (operator
                             prop1
                             prop2)
  "Add relative property.")

(cl-defmethod occ-add-ineq ((operator symbol)
                            (prop1    symbol)
                            (prop2    symbol))
  "Add relative property."
  (cl-assert (memq operator '(gt lt eq)))
  (cl-pushnew (list operator prop1 prop2)
              occ-ineqs))


(defun eval-ineq (ineq)
  (let ((op   (nth 0 ineq))
        (exp1 (nth 1 ineq))
        (exp2 (nth 2 ineq)))
    (cond ((eq op '>) (list (eval-ineq exp1)
                            (list '+ (eval-ineq exp2) 10)))
          ((eq op '<) (list (eval-ineq exp2)
                            (list '+ (eval-ineq exp1) 10)))
          ((eq op '*) (list '*
                            (eval-ineq exp1)
                            exp2)))))

'(('> a b)
  ('< c a)
  ('> e (* a 4)))


;; (defun normalize-inequalities (ineqs)
;;   (let* ((vars (remove-duplicates (flatten ineqs)))
;;          (graph (make-hash-table :test 'equal))
;;          (visited (make-hash-table :test 'equal))
;;          (result '()))
;;     ;; Build a graph of the inequalities
;;     (dolist (var vars)
;;       (setf (gethash var graph) '())
;;       (setf (gethash var visited) nil))
;;     (dolist (ineq ineqs)
;;       (let ((x (cl-first ineq))
;;             (y (second ineq)))
;;         (push y (gethash x graph))))
;;     ;; Check for cycles in the graph
;;     (letrec ((dfs-visit (lambda (var)
;;                           (when (gethash var visited)
;;                             (error "Cycle detected in inequalities"))
;;                           (setf (gethash var visited) t)
;;                           (dolist (dep (gethash var graph))
;;                             (funcall dfs-visit dep))
;;                           (push var result))))
;;       (dolist (var vars)
;;         (funcall dfs-visit var)))
;;     ;; Generate the output list
;;     (mapcar (lambda (var) (find var vars :test 'equal))
;;             (reverse result))))

;; (normalize-inequalities '((> a b) (> b c) (> c (* 2 d)) (> d e)))




(defun normalize-inequalities (ineqs)
  (let* ((vars (delete-dups (apply #'append ineqs)))
         (graph (make-hash-table :test #'equal))
         (visited (make-hash-table :test #'equal))
         (result '()))
    ;; Build a graph of the inequalities
    (dolist (var vars)
      (puthash var '() graph)
      (puthash var nil visited))
    (dolist (ineq ineqs)
      (let ((x (car ineq))
            (y (cadr ineq)))
        (push y (gethash x graph))))
    ;; Check for cycles in the graph
    (letrec ((dfs-visit (lambda (var)
                          (unless (eq (gethash var visited) :active)
                            (puthash var :active visited)
                            (dolist (dep (gethash var graph))
                              (funcall dfs-visit dep))
                            (puthash var t visited)
                            (push var result)))))
      (dolist (var vars)
        (unless (gethash var visited)
          (funcall dfs-visit var))))
    ;; Generate the output list
    (mapcar (lambda (var) (cl-find var vars :test #'equal))
            (reverse result))))

;; (normalize-inequalities '((> a b) (> b c) (> c (* 2 d)) (> d e)))
;; Output: (a b c (* 2 d) (* 2 e))




(defun normalize-inequalities (ineqs)
  (let* ((vars (delete-dups (apply #'append ineqs)))
         (graph (make-hash-table :test #'equal))
         (visited (make-hash-table :test #'equal))
         (result '()))
    ;; Build a graph of the inequalities
    (dolist (var vars)
      (puthash var '() graph)
      (puthash var nil visited))
    (dolist (ineq ineqs)
      (let ((x (car ineq))
            (y (cadr ineq)))
        (push y (gethash x graph))))
    ;; Check for cycles in the graph
    (letrec ((dfs-visit (lambda (var)
                          (unless (eq (gethash var visited) :active)
                            (puthash var :active visited)
                            (dolist (dep (gethash var graph))
                              (funcall dfs-visit dep))
                            (puthash var t visited)
                            (push var result)))))
      (dolist (var vars)
        (unless (gethash var visited)
          (funcall dfs-visit var))))
    ;; Generate the output list
    (mapcar (lambda (var) (cl-find var vars :test #'equal))
            (reverse result))))

;; (normalize-inequalities '((> a b) (> b c) (> c (* 2 d)) (> d e)))
;; Output: (a b c (* 2 d) (* 2 e))




(defun normalize-inequalities (ineqs)
  (let* ((vars (delete-dups (apply #'append ineqs)))
         (graph (make-hash-table :test #'equal))
         (visited (make-hash-table :test #'equal))
         (result '()))
    ;; Build a graph of the inequalities
    (dolist (var vars)
      (puthash var '() graph)
      (puthash var nil visited))
    (dolist (ineq ineqs)
      (let ((lhs (car ineq))
            (rhs (cadr ineq)))
        (cond
          ((and (numberp lhs) (symbolp rhs))
           (push rhs (gethash lhs graph)))
          ((and (numberp lhs) (consp rhs) (eq (car rhs) '*))
           (let ((coeff (cadr rhs))
                 (var (caddr rhs)))
             (push var (gethash lhs graph))
             (push (* coeff var) (gethash lhs graph))))
          ((and (symbolp lhs) (numberp rhs))
           (push lhs (gethash rhs graph)))
          ((and (symbolp lhs) (consp rhs) (eq (car rhs) '*))
           (let ((coeff (cadr rhs))
                 (var (caddr rhs)))
             (push lhs (gethash (* coeff var) graph))
             (push var (gethash (* coeff var) graph))))
          (t (error "Invalid inequality: %s" ineq)))))

    ;; Check for cycles in the graph
    (letrec ((dfs-visit (lambda (var)
                          (unless (eq (gethash var visited) :active)
                            (puthash var :active visited)
                            (dolist (dep (gethash var graph))
                              (funcall dfs-visit dep))
                            (puthash var t visited)
                            (push var result)))))
      (dolist (var vars)
        (unless (gethash var visited)
          (funcall dfs-visit var))))
    ;; Generate the output list
    (mapcar (lambda (var) (cl-find var vars :test #'equal))
            (reverse result))))


(defun normalize-inequalities (inequalities)
  (let* ((var-list (sort (cl-remove-duplicates (mapcan #'(lambda (ineq)
                                                           (list (nth 1 ineq)
                                                                 (nth 2 ineq)))
                                                       inequalities))
                         #'string<))
         (vars (make-hash-table :test #'equal))
         (graph (make-hash-table :test #'equal))
         (degree (make-hash-table :test #'equal))
         (topo-sorted-vars nil))
    ;; initialize hash tables
    (dolist (var var-list)
      (puthash var 0 degree)
      (puthash var nil vars))
    ;; create directed graph from inequalities
    (dolist (ineq inequalities)
      (let ((from (nth 1 ineq))
            (to (nth 2 ineq)))
        (puthash from (cons to (gethash from graph)) graph)
        (puthash to nil vars)))
    ;; calculate in-degree of each variable
    (maphash #'(lambda (var deps)
                 (ignore var)
                 (dolist (dep deps)
                   (puthash dep (1+ (gethash dep degree)) degree)
                   graph
                     ;; perform topological sort on graph
                   (let ((queue ()))
                     (maphash (lambda (var deg)
                                (when (= deg 0)
                                  (push var queue)))
                              degree)
                     (while queue
                       (let ((var (pop queue)))
                         (push var topo-sorted-vars)
                         (dolist (dep (gethash var graph))
                           (let ((new-degree (1- (gethash dep degree))))
                             (puthash dep new-degree degree)
                             (when (= new-degree 0)
                               (push dep queue)))))))
                     ;; construct normalized list of variables
                   (let ((result nil))
                     (dolist (var topo-sorted-vars)
                       (let ((val (gethash var vars)))
                         (if (null val)
                             (error "Cycle detected in inequalities")
                           (let ((multiplier (if (numberp val) val 1)))
                             (push (* multiplier (intern var)) result))))))
                   (nreverse result))))))


;; (normalize-inequalities '((> a b) (> b c) (> c (* 2 d)) (> d e)))
;; Output: (a b c (* 2 d) (* 2 e))

;; (normalize-inequalities '((> a b) (> b c) (> c (* 2 d)) (> d (* 2 e))))
;; Output: (a b c d (* 2 e))


;;; occ-normalize-ineqs.el ends here
