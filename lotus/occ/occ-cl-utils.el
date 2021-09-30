;;; occ-cl-utils.el --- occ cl utils                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(provide 'occ-cl-utils)


(eval-when-compile
  (require 'occ-macros))
(eval-when-compile
  (require 'pcase))
(require 'cl-macs)
(require 'cl-generic)


(occ-testing ;; required
  (progn

    (setq tsk-test (occ-obj-make-tsk-at-point))

    (cl-structure-class cl-struct-occ-tree-tsk)

    (cl-structure-class- cl-struct-occ-tree-tsk)

    (cl-structure-class tsk-test)

    (cl--struct-get-class tsk-test)

    (cl--struct-class-slots cl-struct-occ-tree-tsk)

    (cl--struct-get-class cl-struct-occ-tree-tsk)

    (cl--struct-class-slots cl-struct-cl-structure-class)

    (cl-structure-class-parents cl-struct-cl-structure-class)

    (cl-struct-slot-value 'cl-structure-class 'parents cl-struct-cl-structure-class)

    (cl-struct-slot-value 'cl-structure-class 'parents cl-struct-occ-tree-tsk)

    (cl--struct-get-class tsk-test)

    (cl-struct-slot-value 'cl-structure-class 'name (symbol-value (aref tsk-test 0)))

    (cl--struct-class-slots (symbol-value (aref tsk-test 0)))


    (occ-cl-class-parent-names (occ-cl-class tsk-test))

    (occ-cl-inst-class-parent-names tsk-test)

    (occ-obj-operations-for-prop tsk-test 'root)))


(defun occ-flatten (L)
  ;; https://stackoverflow.com/a/19967639
  "Converts a list to single level."
  (if (null L)
      nil
    (if (atom (first L))
        (cons (first L) (occ-flatten (rest L)))
      (append (occ-flatten (first L)) (occ-flatten (rest L))))))


;; (setq test-xyz (make-occ-ctx))

;; (cl--struct-class-slots
;;  (cl--struct-get-class 'occ-ctx))

;; (cl--struct-class-slots
;;  (cl--struct-get-class 'cl-structure-class))



(defun occ-cl-class (inst)

  ;; BUG: TODO: Improve it.

  (let* ((class-sym (aref inst 0))
         (class-sym (if (boundp class-sym)
                        class-sym
                      (intern (concat "cl-struct-" (symbol-name class-sym))))))
    (if (boundp class-sym)
        (symbol-value class-sym)
        (cl--struct-get-class (aref inst 0)))))

(defun occ-cl-classname (class)
  (cl-struct-slot-value 'cl-structure-class
                        'name
                        class))

;; (cl-struct-slot-value 'cl-structure-class
;;                       'parents
;;                       (cl--struct-get-class 'occ-ctx))

;; (occ-cl-class-parents (occ-cl-class test-xyz))
;; (occ-cl-class-parent-names (cl--struct-get-class 'occ-tsk))


(defun occ-cl-class-parents (class)
  (cl-struct-slot-value 'cl-structure-class
                        'parents
                        class))

(defun occ-cl-class-parent-names (class)
  (mapcar #'(lambda (parent)
              (cons (cl-struct-slot-value 'cl-structure-class
                                          'name
                                          parent)
                    (when parent (occ-cl-class-parent-names parent))))
          (occ-cl-class-parents class)))


(defun occ-cl-inst-classname (inst)
  (occ-cl-classname (occ-cl-class inst)))

(defun occ-cl-inst-class-parent-names (inst)
  (occ-flatten (occ-cl-class-parent-names (occ-cl-class inst))))

(defun occ-cl-inst-class-names (inst)
  (cons (occ-cl-inst-classname inst)
        (occ-flatten (occ-cl-class-parent-names (occ-cl-class inst)))))


(defun occ-cl-get-field (object field)
  (cl-struct-slot-value (occ-cl-inst-classname object) field object))
(defun occ-cl-set-field (object field value)
  (setf (cl-struct-slot-value (occ-cl-inst-classname object) field object) value))
(defun occ-cl-get-fields (object fields)
  (mapcar #'(lambda (field)
              (cons field
                    (occ-cl-get-field object field)))
          fileds))
(defun occ-cl-class-slots (class)
  (mapcar #'(lambda (slot) (aref slot 1))
          (cl--struct-class-slots (cl--struct-get-class class))))
;; (defun cl-class-slot-value (obj slot)
;;   (when (member slot (occ-cl-class-slots (occ-cl-inst-classname obj)))
;;     (cl-struct-slot-value (occ-cl-inst-classname obj) slot obj)))
(defun occ-cl-class-obj-slot-value (class slot obj)
  (when (member slot (occ-cl-class-slots class))
    (cl-struct-slot-value class slot
                          obj)))
(defun occ-cl-obj-slot-value (obj slot)
  (occ-cl-class-obj-slot-value (occ-cl-inst-classname obj) slot obj))
(defun occ-cl-obj-plist-value (obj)
  (occ-cl-obj-slot-value obj 'plist))


(cl-defun occ-cl-method-param-signs (method)
  "Get params signatures for all defined methods"
  (let ((method-instances (cl--generic method)))
    (mapcar #'(lambda (x) (aref x 1))
            (if method-instances
                (aref method-instances 3)))))

(cl-defun occ-cl-method-param-case (signature-val-spec)
  "signature-val-spec = (METHOD (PARAMS VAL))"
  (cl-destructuring-bind (method (param-spec val)) signature-val-spec
    (remove nil
            (mapcar #'(lambda (fspec)
                        (funcall `(lambda ()
                                    (pcase ',fspec
                                      (,param-spec ,val)
                                      (_ nil)))))
                    (occ-cl-method-param-signs method)))))

(cl-defun occ-cl-method-param-case-with-value (signature-val-spec obj)
  "signature-val-spec = (METHOD PARAMS VAL)"
  (cl-destructuring-bind (method (param-spec val)) signature-val-spec
    (remove nil
            (mapcar #'(lambda (fspec)
                        (let ((first-arg (funcall `(lambda ()
                                                     (pcase ',fspec
                                                       (,param-spec ,val)
                                                       (_ nil))))))
                          (when (and first-arg
                                     (funcall method (cons first-arg
                                                           obj)))
                            first-arg)))
                    (occ-cl-method-param-signs method)))))

(defun occ-cl-method-param-case-with-value-new (signature-val-spec obj)
  "signature-val-spec = (METHOD PARAMS VAL)"
  (cl-destructuring-bind (method (param-spec val)) signature-val-spec
    (remove nil
            (mapcar #'(lambda (fspec)
                        (let ((first-arg (funcall `(lambda ()
                                                     (pcase ',fspec
                                                       (,param-spec ,val)
                                                       (_ nil))))))
                          (when (and first-arg
                                     ;; (funcall method (cons first-arg obj))) -- TODO BUG make it general
                                     (funcall method obj first-arg))
                            first-arg)))
                    (occ-cl-method-param-signs method)))))

(defun occ-cl-method-arg-get (method fn)
  (mapcar fn
          (occ-cl-method-param-signs method)))

(defun occ-cl-method-first-arg (method)
  (mapcar #'(lambda (fspec) (cadar fspec))
          (occ-cl-method-param-signs method)))

(defun occ-cl-method-first-arg (method)
  (occ-cl-method-arg-get method #'cadar))

(defun occ-cl-method-first-arg-with-value (method obj)
  (mapcar #'(lambda (fspec)
              (let ((first-arg (cadar fspec)))
                (when (funcall method (cons first-arg obj)) first-arg)))
          (occ-cl-method-param-signs method)))


(defun occ-cl-method-param-values (method param-exp val)
  (funcall `(lambda ()
              (occ-cl-method-param-case '(,method (,param-exp ,val))))))

(defun occ-cl-collect-on-classes (fn inst)
  (mapcan #'(lambda (class)
              (funcall fn class))
          (occ-cl-inst-class-names inst)))

;;; occ-cl-utils.el ends here
