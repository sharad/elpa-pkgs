;;; occ-obj-common.el --- occ-api               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
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

(provide 'occ-obj-common)


(require 'occ-obj)
(require 'occ-prop-intf)


;; TODO org-base-buffer

;; https://stackoverflow.com/questions/12262220/add-created-date-property-to-todos-in-org-mode

;; "org tsks accss common api"

(defun occ-plist-get (plist prop)
  (let ((key (sym2key prop)))
    (if key
        (plist-get plist
                   (sym2key prop))
      (occ-error "occ-plist-get: Can not make keyword for `'%s'" prop))))

(defmacro occ-plist-set (plist prop value)
  `(let ((key (sym2key ,prop)))
     (cl-assert (evenp (length ,plist)))
     (if key
         (plist-put ,plist ;TODO ??? (occ-cl-obj-plist-value obj)
                    key ,value)
       (occ-error "occ-plist-set: Can not make keyword for `'%s'" ,prop))))

(defun occ-list-get-evens (lst)
  (cond
   ((null lst) nil)
   (t          (cons (first lst)
                     (occ-list-get-evens (nthcdr 2 lst))))))

;; (defun list-get-odds (lst)
;;   (cond
;;    ((null lst) nil)
;;    ( t (cons  (nth 1 lst) (list-get-odds (rest (rest lst)))))))

(defun occ-plist-get-keys (plist)
  (occ-list-get-evens plist))


(cl-defgeneric occ-obj-get-property-internal (obj
                                              prop)
  "get property of object")
(cl-defgeneric occ-obj-get-property (obj
                                     prop)
  "get property of object")
(cl-defgeneric occ-obj-get-properties-internal (obj
                                                props)
  "get property of object")
(cl-defgeneric occ-obj-get-properties (obj
                                       props)
  "get property of object")

(cl-defmethod occ-obj-get-property-internal ((obj occ-obj)
                                             (prop symbol))
  ;; mainly used by occ-tsk only.
  (occ-debug "(OCC-OBJ-GET-PROPERTY (OBJ OCC-OBJ)): calling for prop %s" prop)
  (if (memq prop
            (occ-cl-class-slots (occ-cl-inst-classname obj)))
      (occ-cl-get-field obj prop)
    (or (occ-plist-get (occ-cl-obj-plist-value obj)
                       prop)
        (occ-plist-get (occ-cl-obj-plist-value obj)
                       (upcase-sym prop)))))

(cl-defmethod occ-obj-get-properties-internal ((obj   occ-obj)
                                               (props list))
  ;; mainly used by occ-tsk only.
  (mapcar #'(lambda (prop)
              (cons prop (occ-obj-get-property obj prop)))
          props))


(cl-defmethod occ-obj-get-property ((obj  occ-obj-tsk)
                                    (prop symbol))
  "Return occ compatible value of prop PROP from OCC-CTX OBJ."
  (occ-debug "(OCC-OBJ-GET-PROPERTY (OBJ OCC-OBJ-TSK)): calling for prop %s" prop)
  (occ-obj-get-property-internal (occ-obj-tsk obj) prop))

(cl-defmethod occ-obj-get-property ((obj  occ-obj-ctx-tsk)
                                    (prop symbol))
  "Return occ compatible value of prop PROP from OCC-CTX OBJ."
  (occ-debug "(OCC-OBJ-GET-PROPERTY (OBJ OCC-OBJ-CTX-TSK)): calling for prop %s" prop)
  (occ-obj-get-property (occ-obj-tsk obj) prop))

(cl-defmethod occ-obj-get-property ((obj  occ-obj-ctx)
                                    (prop symbol))
  "Return occ compatible value of prop PROP from OCC-CTX OBJ."
  (occ-debug "(OCC-OBJ-GET-PROPERTY (OBJ OCC-OBJ-CTX)): calling for prop %s" prop)
  (occ-obj-get-property-value-from-ctx (occ-obj-ctx obj)
                               prop))


(cl-defmethod occ-obj-get-properties ((obj   occ-obj-tsk)
                                      (props list))
  ;; mainly used by occ-tsk only.
  (occ-obj-get-properties-internal (occ-obj-tsk obj) props))

(cl-defmethod occ-obj-get-properties ((obj   occ-obj-ctx-tsk)
                                      (props list))
  ;; mainly used by occ-tsk only.
  (occ-obj-get-properties (occ-obj-tsk obj) props))

(cl-defmethod occ-obj-get-properties ((obj   occ-obj-ctx)
                                      (props list))
  ;; mainly used by occ-tsk only.
  (occ-obj-get-properties (occ-obj-ctx obj) props))



(cl-defmethod occ-obj-set-property ((obj occ-obj)
                                    prop
                                    value
                                    &key not-recursive)
  ;; mainly used by occ-tsk only
  (occ-message "(occ-obj-set-property occ-obj): prop %s, value %s" (prin1-to-string prop) (prin1-to-string value))
  (if (memq prop
            (occ-cl-class-slots (occ-cl-inst-classname obj)))
      (setf (cl-struct-slot-value (occ-cl-inst-classname obj) prop obj)
            value)
    (let ((plist-prop (if (occ-plist-get (occ-cl-obj-plist-value obj)
                                         prop)
                          prop
                        (upcase-sym prop))))
      (occ-debug "(occ-obj-set-property occ-obj): plist got %s using %s"
                 prop plist-prop)
      (occ-plist-set
       ;; NOTE: as Property block keys return by (org-element-at-point) are in
       ;; UPCASE even in actual org file it is lower or camel case. so our obj
       ;; (tsk) also must have to be in line of it as it also got created with
       ;; same function (org-element-at-point).
       (cl-struct-slot-value (occ-cl-inst-classname obj)
                             'plist
                             obj)
       plist-prop value))))

(cl-defmethod occ-obj-set-property ((obj occ-tree-tsk)
                                    prop
                                    value &key not-recursive)
  ;; TODO: do it recursively.
  ;; mainly used by occ-tsk only
  (occ-message "(occ-obj-set-property (obj occ-tree-tsk)) prop %s, value %s" (prin1-to-string prop) (prin1-to-string value))
  (cl-call-next-method)
  (when not-recursive
    (dolist (subtsk (occ-tree-tsk-subtree (occ-obj-tsk obj)))
      (occ-obj-set-property subtsk
                        prop
                        value
                        :not-recursive not-recursive))))


(cl-defmethod occ-obj-set-property ((obj occ-obj-tsk)
                                    prop
                                    value &key not-recursive)
  (occ-message "(occ-obj-set-property (obj occ-obj-tsk)) prop %s, value %s" (prin1-to-string prop) (prin1-to-string value))
  (cl-call-next-method))

(cl-defmethod occ-obj-set-property ((obj occ-obj-ctx-tsk)
                                    prop
                                    value &key not-recursive)
  (occ-message "(occ-obj-set-property (obj occ-obj-ctx-tsk)) prop %s, value %s" (prin1-to-string prop) (prin1-to-string value))
  (occ-obj-set-property (occ-obj-tsk obj) prop
                    value :not-recursive not-recursive))

(cl-defmethod occ-obj-set-property ((obj occ-obj-ctx)
                                    prop
                                    value &key not-recursive)
  (occ-message "(occ-obj-set-property (obj occ-obj-ctx)) prop %s, value %s" (prin1-to-string prop) (prin1-to-string value))
  (occ-obj-set-property (occ-obj-ctx obj) prop
                    value :not-recursive not-recursive))


(cl-defmethod occ-obj-class-slots ((obj occ-obj))
  (let* ((plist      (occ-cl-obj-plist-value obj))
         (plist-keys (occ-plist-get-keys plist))
         (slots      (occ-cl-class-slots (occ-cl-inst-classname obj))))
    (append slots
            (mapcar #'key2sym plist-keys))))
(cl-defmethod occ-obj-defined-slots ((obj occ-obj))
  (let* ((plist      (occ-cl-obj-plist-value obj))
         (plist-keys (occ-plist-get-keys plist))
         (slots      (append (occ-cl-class-slots (occ-cl-inst-classname obj))
                             (mapcar #'key2sym
                                     plist-keys))))
    slots))
(cl-defmethod occ-obj-defined-slots-with-value ((obj occ-obj))
  (let* ((slots (occ-obj-defined-slots obj)))
    (remove-if-not #'(lambda (slot)
                       (occ-obj-get-property obj slot))
                   slots)))
(cl-defmethod occ-obj-cl-method-matched-arg ((method symbol)
                                             (ctx symbol))
  (occ-cl-method-first-arg method))
(cl-defmethod occ-obj-cl-method-matched-arg ((method symbol)
                                             (ctx occ-ctx))
  (let ((slots (occ-obj-defined-slots-with-value ctx)))
    (remove-if-not #'(lambda (arg) (memq arg slots))
                   (occ-cl-method-first-arg method))))
(cl-defmethod occ-obj-cl-method-matched-arg ((method1 symbol)
                                             (method2 symbol)
                                             (ctx occ-ctx))
  (let ((slots (occ-cl-method-first-arg-with-value method2
                                               ctx)))
    (remove-if-not #'(lambda (arg) (memq arg slots))
                   (occ-cl-method-first-arg method1))))


(cl-defgeneric occ-obj-cl-method-sig-matched-arg (method-sig
                                                  ctx)
  "test")
(cl-defmethod occ-obj-cl-method-sig-matched-arg ((method-sig cons)
                                                 (ctx symbol))
  (occ-cl-method-param-case method-sig))
(cl-defmethod occ-obj-cl-method-sig-matched-arg ((method-sig cons)
                                                 (ctx occ-ctx))
  (let ((slots (occ-obj-defined-slots-with-value-new ctx)))
    (remove-if-not #'(lambda (arg) (memq arg slots))
                   (occ-cl-method-param-case method-sig))))
(cl-defmethod occ-obj-cl-method-sigs-matched-arg ((method-sig1 cons)
                                                  (method-sig2 cons)
                                                  (ctx occ-ctx))
  (let ((slots (occ-cl-method-param-case-with-value-new method-sig2 ctx)))
    (remove-if-not #'(lambda (arg) (memq arg slots))
                   (occ-cl-method-param-case method-sig1))))

;;; occ-obj-common.el ends here
