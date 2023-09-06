;;; occ-impl.el --- OCC implementation generics and general default methods  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Music Player Daemon (MPD) user

;; Author: s <>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(provide 'occ-impl)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-macros)


;; (cl-defgeneric occ-obj-rankprop (obj
(cl-defgeneric occ-obj-impl-rank (obj
                                  property)
  "Return the RANK (number) for OBJ based on the property PROPERTY")
(cl-defmethod occ-obj-impl-rank (obj
                                 property)
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  ;; too much output
  ;; (occ-debug "occ-obj-rank(tsk-pair=%s ctx=%s)" tsk-pair ctx)
  (occ-debug "occ-obj-impl-rank(obj=%s symbol=%s)" (occ-obj-format obj) property)
  0)
(cl-defmethod occ-obj-impl-rank ((obj  occ-tsk)
                                 (property symbol))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  (occ-debug "occ-obj-impl-rank(obj=%s symbol=%s)"
             (occ-obj-format obj)
             property)
  0)
(cl-defmethod occ-obj-impl-rank ((obj  occ-obj-ctx-tsk)
                                 (property symbol))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  (occ-debug "occ-obj-impl-rank(obj=%s symbol=%s)" (occ-obj-format obj) property)
  (occ-obj-impl-rank obj property))


;; adding base propetry NIL for helping to add relative properties
(cl-defmethod occ-obj-impl-rank ((obj  occ-tsk)
                                 (property (eql nil)))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  (occ-debug "occ-obj-impl-rank(obj=%s symbol=%s)"
             (occ-obj-format obj)
             property)
  0)
(cl-defmethod occ-obj-impl-rank ((obj  occ-obj-ctx-tsk)
                                 (property (eql nil)))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  (occ-debug "occ-obj-impl-rank(obj=%s symbol=%s)" (occ-obj-format obj) property)
  (occ-obj-impl-rank obj property))


;; (cl-defgeneric occ-obj-impl-has-p (obj
(cl-defgeneric occ-obj-impl-prop= (property
                                   prop-value
                                   value)
  "OBJ has property PROPERTY")
(cl-defmethod occ-obj-impl-prop= ((prop symbol)
                                  prop-value
                                  value)
  "OBJ has property PROPERTY"
  (equal prop-value
         value))


;; (cl-defgeneric occ-obj-impl-has-p (obj
(cl-defgeneric occ-obj-impl-matches (obj
                                     property
                                     value)
  "VALUE equal prop-value of OBJ for PROPERTY")
(cl-defmethod occ-obj-impl-matches ((obj occ-obj-tsk)
                                    (prop symbol)
                                    value)
  "VALUE equal prop-value of OBJ for PROPERTY"
  (let* ((tsk            (occ-obj-tsk obj))
         (prop-value (occ-obj-get-property tsk prop)))
    (if (occ-obj-list-p obj prop)
        (cl-remove-if-not #'(lambda (pvalue)
                              (occ-obj-intf-prop= prop
                                                  pvalue
                                                  value))
                 prop-value)
      (when (occ-obj-intf-prop= prop
                              (occ-obj-get-property obj prop)
                              value)
        prop-value))))


;; (cl-defgeneric occ-obj-impl-has-p (obj
(cl-defgeneric occ-obj-impl-has-p (obj
                                   property
                                   value)
  "OBJ has property PROPERTY")
(cl-defmethod occ-obj-impl-has-p ((obj occ-obj-tsk)
                                  (prop symbol)
                                  value)
  "OBJ has property PROPERTY"
  (let* ((tsk        (occ-obj-tsk obj))
         (prop-value (occ-obj-get-property tsk prop)))
    (if (occ-obj-list-p obj prop)
        (cl-some #'(lambda (pvalue)
                     (occ-obj-intf-prop= prop
                                         pvalue
                                         value))
                 prop-value)
      (occ-obj-intf-prop= prop
                          (occ-obj-get-property obj prop)
                          value))))


(cl-defgeneric occ-obj-impl-get (ctx
                                 property
                                 obj)
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ.")
  ;; (occ-error "must return occ compatible value.")


;; (cl-defgeneric occ-obj-impl-format-prop (obj
(cl-defgeneric occ-obj-impl-format (obj
                                    property
                                    value)
  "Return format printable value of property PROPERTY."
  (ignore obj)
  (ignore property)
  value)


;; ;; (cl-defgeneric occ-obj-impl-list-p (property)
;; (cl-defgeneric occ-obj-impl-list-p (property)
;;   "Is the property PROPERTY has VALUES in list, Method tell
;;    property represent list or not.")

;; (cl-defmethod occ-obj-impl-list-p ((property symbol))
;;   "Is the property PROPERTY has VALUES in list, Method tell
;;    property represent list or not."
;;   ;; 'list
;;   ;; (occ-error "Implement method occ-obj-impl-list-p for property %s" property)
;;   (occ-debug "occ-obj-impl-list-p: no method for property %s using default."
;;              property)
;;   nil)


;; (cl-defgeneric occ-obj-impl-list-p (property)
(cl-defgeneric occ-obj-impl-list-p (mrk
                                    property)
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not.")

(cl-defmethod occ-obj-impl-list-p ((mrk marker)
                                   (property symbol))
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not."
  ;; 'list
  ;; (occ-error "Implement method occ-obj-impl-list-p for property %s" property)
  (occ-debug "occ-obj-impl-list-p: no method for property %s using default."
             property)
  nil)

(cl-defmethod occ-obj-impl-list-p ((ctx occ-obj-ctx)
                                   (property symbol))
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not."
  ;; 'list
  ;; (occ-error "Implement method occ-obj-impl-list-p for property %s" property)
  (occ-debug "occ-obj-impl-list-p: no method for property %s using default."
             property)
  nil)


;; (cl-defmethod occ-obj-impl-list-p ((obj (eql operation))
;;                               (operation symbol))
;;   "Is the property PROPERTY has VALUES in list, Method tell
;;    property represent list or not."
;;   (occ-obj-intf-list-p obj
;;                        operation))



;; (cl-defgeneric occ-obj-impl-list-p (property)
(cl-defgeneric occ-obj-impl-list-p (ctx property)
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not.")

(cl-defmethod occ-obj-impl-list-p ((tsk occ-obj-ctx)
                                   (property symbol))
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not."
  ;; 'list
  ;; (occ-error "Implement method occ-obj-impl-list-p for property %s" property)
  (occ-debug "occ-obj-impl-list-p: no method for property %s using default."
             property)
  nil)


;; (cl-defgeneric occ-obj-impl-to-org (property
(cl-defgeneric occ-obj-impl-to-org (property
                                    value)
  "Return string representation for property PROPERTY, Method
convert value VALUE of property PROPERTY from occ to org string
representation.")
;; (cl-defmethod occ-obj-impl-to-org ((property symbol)
(cl-defmethod occ-obj-impl-to-org ((property symbol)
                                   value)
  "Return string representation for property PROPERTY, Method
convert value VALUE of property PROPERTY from occ to org string
representation."
  ;; (occ-error "Implement method occ-obj-impl-to-org for property %s" property)
  (occ-debug "occ-obj-impl-to-org: no method for property %s using default."
             property)
  value)


;; (cl-defgeneric occ-obj-impl-from-org (property
(cl-defgeneric occ-obj-impl-from-org (property
                                      value)
  "Return the Actual Object representation for property
PROPERTY, Method convert value VALUE of property PROPERTY from
org string to occ representation.")
;; (cl-defmethod occ-obj-impl-from-org ((property symbol)
(cl-defmethod occ-obj-impl-from-org ((property symbol)
                                     value)
  "Return the Actual Object representation for property
PROPERTY, Method convert value VALUE of property PROPERTY from
org string to occ representation."
  ;; (occ-error "Implement method occ-obj-impl-from-org for property %s" property)
  (occ-debug "occ-obj-impl-from-org: no method for property %s using default." property)
  value)
;; (cl-defmethod occ-obj-impl-from-org ((property symbol)
(cl-defmethod occ-obj-impl-from-org ((property symbol)
                                     (value string))
  "Return the Actual Object representation for property
PROPERTY, Method convert value VALUE of property PROPERTY from
org string to occ representation."
  ;; (occ-error "Implement method occ-obj-impl-from-org for property %s" property)
  (occ-debug "occ-obj-impl-from-org: no method for property %s using default." property)
  value)


(cl-defmethod occ-obj-impl-get ((user occ-user-agent)
                                (property symbol)
                                (ctsk occ-obj-tsk))
  "Read value of list of elements if (occ-obj-impl-list-p PROPERTY) else
element for property PROPERTY from user for OCC-TSK OBJ, must
return ORG compatible value."
  (ignore obj)
  (occ-error "Implement method occ-obj-impl-readprop-from-user for property %s" property))


;; (cl-defgeneric occ-obj-impl-require-p (obj
(cl-defgeneric occ-obj-impl-require-p (obj
                                       operation
                                       property
                                       values)
  "Used by OCC-OBJ-IMPL-GEN-EDIT-IF-REQUIRED to decide for this property
_TEMPLATE_ if CALLABLE (helm method) should be generated."
  (ignore obj)
  (ignore operation)
  (ignore property)
  (ignore values)
  (occ-debug "occ-obj-impl-require-p0 is called"))
(cl-defmethod occ-obj-impl-require-p ((obj occ-obj-tsk)
                                      (operation (eql _operation_))
                                      (property  symbol)
                                      values)
  "Used by OCC-OBJ-IMPL-GEN-EDIT-IF-REQUIRED to decide for this property
_TEMPLATE_ if CALLABLE (helm method) should be generated."
  (ignore obj)
  (ignore operation)
  (ignore property)
  (ignore values)
  (occ-debug "occ-obj-impl-require-p1 is called")
  t)


;; (cl-defgeneric occ-obj-impl-prop-default-value (obj
(cl-defgeneric occ-obj-impl-default (obj
                                     property
                                     operation)
  "Return a default VALUE of property _TEMPLATE_.")
(cl-defmethod occ-obj-impl-default ((obj occ-obj-tsk)
                                    (property symbol)
                                    (operation symbol))
  "Return a default VALUE of property _TEMPLATE_."
  (ignore obj)
  (ignore property)
  (ignore operation)
  nil)
(cl-defmethod occ-obj-impl-default ((obj occ-obj-ctx-tsk)
                                    (property symbol)
                                    (operation symbol))
  "Return a default VALUE of property _TEMPLATE_."
  (ignore operation)

  ;; (or (and (not (occ-obj-list-p 'operation operation))
  ;;          (and (occ-obj-list-p (occ-obj-tsk obj) property)
  ;;               (occ-obj-list-p (occ-obj-ctx obj) property))
  ;;          (not (or (occ-obj-list-p (occ-obj-tsk obj) property)
  ;;                   (occ-obj-list-p (occ-obj-ctx obj) property))))
  ;;     (and (occ-obj-list-p 'operation operation)
  ;;          (occ-obj-list-p (occ-obj-tsk obj) property)
  ;;          (not (occ-obj-list-p (occ-obj-ctx obj) property))))

  (if (occ-obj-operation-valid-p (occ-obj-tsk obj)
                                 (occ-obj-ctx obj)
                                 property
                                 operation)
      (occ-obj-get-property (occ-obj-ctx obj)
                            property)))

;; (cl-defmethod occ-obj-impl-default ((obj occ-obj-ctx-tsk)
;;                                     (property symbol)
;;                                     (operation symbol))
;;   "Return a default VALUE of property _TEMPLATE_."
;;   (ignore operation)
;;   (occ-obj-get-property (occ-obj-ctx obj)
;;                         property))


(cl-defmethod occ-obj-impl-values ((tsk occ-obj-tsk)
                                   (ctx occ-obj-ctx)
                                   (property symbol)
                                   (operation symbol)
                                   func)
  (let ((tsk   (occ-obj-tsk tsk))
        (ctx   (occ-obj-ctx ctx))
        (value (occ-obj-get-property (occ-obj-ctx obj)
                                     property))))
  (if (not (occ-obj-list-p 'operation operation))
      (when (eql (occ-obj-list-p tsk property)
                 (occ-obj-list-p ctx property))
        (list (apply func value)))))

(cl-defmethod occ-obj-impl-values ((tsk occ-obj-tsk)
                                   (ctx occ-obj-ctx)
                                   (property symbol)
                                   (operation (eql add))
                                   func)
  (let ((tsk   (occ-obj-tsk tsk))
        (ctx   (occ-obj-ctx ctx))
        (value (occ-obj-get-property (occ-obj-ctx obj)
                                     property))))
  (if (not (occ-obj-list-p 'operation operation))
      (when (eql (occ-obj-list-p tsk property)
                 (occ-obj-list-p ctx property))
        (list (apply func value)))))

(cl-defmethod occ-obj-impl-values ((tsk occ-obj-tsk)
                                   (ctx occ-obj-ctx)
                                   (property symbol)
                                   (operation (eql remove))
                                   func)
  (let ((tsk   (occ-obj-tsk tsk))
        (ctx   (occ-obj-ctx ctx))
        (value (occ-obj-get-property (occ-obj-ctx obj)
                                     property))))
  (if (not (occ-obj-list-p 'operation operation))
      (when (eql (occ-obj-list-p tsk property)
                 (occ-obj-list-p ctx property))
        (list (apply func value)))))

(cl-defmethod occ-obj-impl-values ((tsk occ-obj-tsk)
                                   (ctx occ-obj-ctx)
                                   (property symbol)
                                   (operation (eql put))
                                   func)
  (let ((tsk   (occ-obj-tsk tsk))
        (ctx   (occ-obj-ctx ctx))
        (value (occ-obj-get-property (occ-obj-ctx obj)
                                     property))))
  (if (not (occ-obj-list-p 'operation operation))
      (when (eql (occ-obj-list-p tsk property)
                 (occ-obj-list-p ctx property))
        (list (apply func value)))))

(cl-defmethod occ-obj-impl-values ((tsk occ-obj-tsk)
                                   (ctx occ-obj-ctx)
                                   (property symbol)
                                   (operation (eql delete))
                                   func)
  (let ((tsk   (occ-obj-tsk tsk))
        (ctx   (occ-obj-ctx ctx))
        (value (occ-obj-get-property (occ-obj-ctx obj)
                                     property))))
  (if (not (occ-obj-list-p 'operation operation))
      (when (eql (occ-obj-list-p tsk property)
                 (occ-obj-list-p ctx property))
        (list (apply func value)))))


(cl-defgeneric occ-do-impl-operation (obj
                                      operation
                                      property
                                      value)
  "Do the actual OPERATION.")

(cl-defmethod occ-do-impl-operation (obj
                                     operation
                                     property
                                     value)
  ;; NOTE: obj == marker -- present in builtin.el
  "Do the actual OPERATION."
  (occ-error "Implement it for obj=%s property %s."
             (occ-obj-format obj)
             property))


;; (cl-defgeneric occ-do-impl-checkout-prop (obj
(cl-defgeneric occ-do-impl-checkout (obj
                                     property)
  "Checkout property PROPERTY in case of force clock-in.")
(cl-defmethod occ-do-impl-checkout ((obj occ-obj-tsk)
                                    (property symbol))
  "Checkout property in case of force clock-in."
  (ignore obj)
  (occ-error "Implement it for %s: Checkout property in case of force clock-in." property))


(occ-testing
 (cl-defmethod occ-obj-impl-rank ((obj occ-tsk)
                                  (prop (eql _template_)))
   "Return the RANK (number) for OCC-TSK based on the property _TEMPLATE_"
   (ignore obj)
   (ignore prop))
 (cl-defmethod occ-obj-impl-has-p ((obj occ-obj-tsk)
                                   (property symbol)
                                   value)
   "OBJ-has-property PROPERTY"
   (ignore obj)
   (ignore property)
   (ignore value))
 ;; (cl-defmethod occ-obj-impl-get-property-value-from-ctx ((obj occ-ctx)
 (cl-defmethod occ-obj-impl-get ((ctx occ-ctx)
                                 (property symbol)
                                 arg)
   "Return occ compatible value of property PROPERTY from OCC-CTX OBJ."
   (ignore obj)
   (ignore property)
   (occ-error "must return occ compatible value."))
 (cl-defmethod occ-obj-impl-format ((obj occ-obj-tsk)
                                    (property symbol)
                                    value)
   "Return format printable value of property PROPERTY."
   (ignore obj)
   (ignore property)
   value)
 (cl-defmethod occ-obj-impl-list-p ((prop (eql _template_)))
   "Is the property _TEMPLATE_ has VALUES in list, Method tell
   property represent list or not."
   (ignore prop))
;;  (cl-defmethod occ-obj-impl-readprop-from-user ((obj occ-tsk))
 (cl-defmethod  occ-obj-impl-get ((user occ-user-agent)
                                  (prop (eql _template_))
                                  (ctsk occ-obj-tsk))
   "Read value of list of elements if (occ-obj-list-p CTSK PROPERTY) else
element for property PROPERTY from user for OCC-TSK OBJ, must
return ORG compatible value."
   (ignore obj)
   (ignore prop))
 (cl-defmethod occ-obj-impl-require-p ((obj occ-obj-tsk)
                                       (operation (eql _operation_))
                                       (prop (eql _template_))
                                       values)
   "Used by OCC-OBJ-IMPL-GEN-EDIT-IF-REQUIRED to decide for this property
_TEMPLATE_ if CALLABLE (helm method) should be generated."
   (ignore obj)
   (ignore operation)
   (ignore prop)
   (ignore values)
   (occ-debug "occ-obj-impl-require-p3 is called"))
 (cl-defmethod occ-obj-impl-default ((obj occ-obj-tsk)
                                     (prop (eql _template_))
                                     (operation (eql _operation_)))
   "Return a default VALUE of property _TEMPLATE_."
   (ignore obj)
   (ignore prop)
   (ignore operation))
 (cl-defmethod occ-obj-impl-operation ((obj occ-obj-tsk)
                                       (operation (eql _operation_))
                                       (prop (eql _template_))
                                       values)
   "Do the actual _OPERATION_."
   (ignore obj)
   (ignore operation)
   (ignore prop)
   (ignore values))
 (cl-defmethod occ-do-impl-operation ((obj occ-obj-tsk)
                                      (operation (eql _operation_))
                                      (prop (eql _template_))
                                      values)
   "Do the actual _OPERATION_."
   (ignore obj)
   (ignore operation)
   (ignore prop)
   (ignore values))
 (cl-defmethod occ-do-impl-checkout ((obj occ-obj-tsk)
                                     (prop (eql _template_)))
   "Checkout property _TEMPLATE_ in case of force clock-in."
   (ignore obj)
   (ignore prop)))




;; NOTE: These two around methods not belongs to occ-prop-intf.el
;;       they belongs here only.
(cl-defmethod occ-obj-impl-get :around ((user occ-user-agent)
                                        (prop symbol)
                                        (obj  occ-obj-tsk))
  "Read value of element of list for property PROP from user for
OCC-TSK OBJ."
  (ignore obj)
  (condition-case e ;; if (cl-next-method-p)
      (cl-call-next-method)
    ((cl-no-next-method) (occ-error "No
(cl-defmethod occ-obj-impl-get ((obj occ-obj-tsk) (prop (eql %s)))
  ...)

method provided." prop))))

(cl-defmethod occ-obj-impl-get :around ((user occ-user-agent)
                                        (prop symbol)
                                        (obj  occ-obj-tsk))
  "Read value of element of list for property PROP from user for
OCC-TSK OBJ."
  (ignore obj)
  (condition-case e ;; if (cl-next-method-p)
      (cl-call-next-method)
    ((cl-no-next-method) (occ-error "No
(cl-defmethod occ-obj-impl-get ((obj occ-obj-tsk) (prop (eql %s)))
   ...)

method provided." prop))))


;; (cl-defmethod occ-do-impl-operation :around ((obj       marker)
;;                                              (operation symbol)
;;                                              (prop      symbol)
;;                                              value)
;;   "Accept occ compatible VALUES"
;;   (if (memq operation
;;             (occ-obj-operations-org-operation obj))
;;       (cl-call-next-method)
;;     (occ-error "Define (cl-defmethod occ-do-impl-operation ((pom marker) (operation (eql %s)) (prop (eql %s)) value) ... )"
;;                operation prop)))


(cl-defmethod occ-do-impl-operation :around ((mrk  marker)
                                             (operation symbol)
                                             (prop symbol)
                                             value)
  "Accept org compatible VALUE"
  (occ-message "I should be called first in case of MARKER")
  (lotus-with-marker mrk
    (unless (org-get-property-block)
      ;; create property drawer
      ;; TODO: NOTE: only create property block if 100% sure value is going to be set.
      (occ-debug "occ-do-operation[ :around ]: property block not exist so creating it.")
      (let* ((range (org-get-property-block (point) 'force))
             (start (when (consp range) (1- (cl-first range)))))
        (if (and range
                 start)
            (when (numberp start)
              (goto-char start))
          (occ-error "occ-do-operation[ :around ]: not able to create property block to add property %s: %s"
                     prop
                     value))))

    (if (org-get-property-block)
        (progn
          (occ-debug "occ-do-operation[ :around ]: adding prop: %s value: %s using (org-set-property)."
                     prop
                     value)
          (let ((retval (condition-case e ;; if (cl-next-method-p)
                            (cl-call-next-method mrk
                                                 operation
                                                 prop
                                                 (occ-obj-to-org prop
                                                                 value))
                          ((cl-no-next-method) (occ-error "No
(cl-defmethod occ-do-impl-operation ((pom marker) (operation (eql %s)) (prop (eql %s)) value)
  ...)

method provided." operation prop)))))
            (occ-debug "occ-do-operation: (occ-do-operation mrk) returned %s" retval)
            retval))
        (occ-error "occ-do-operation[ :around ]: can not get property block to add property %s: %s"
                   prop
                   value))))


;;
;;
;;
;; * read prop value from user
;; * read prop value from ctx
;; * read prop value from tsk
;; * write prop value to tsk
;; * write prop value to ctx error
;; * write to user means print
;; * checkout prop value from tsk
;; (org-read-date) (org--deadline-or-schedule arg 'scheduled tim)e

;;; occ-impl.el ends here
