;;; occ-prop-intf.el --- occ property interface      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file should only has interface and default interface methods which is
;; implemented by occ-property-methods.org

;;; Code:

(provide 'occ-prop-intf)


(require 'occ-macros)
(require 'occ-util-common)
(require 'occ-obj)
(require 'occ-prop-utils)


(cl-defgeneric occ-obj-rankprop (obj
                                 property)
  "Return the RANK (number) for OBJ based on the property PROPERTY")
(cl-defmethod occ-obj-rankprop (obj
                                property)
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  ;; too much output
  ;; (occ-debug "occ-obj-rank(tsk-pair=%s ctx=%s)" tsk-pair ctx)
  (occ-debug "occ-obj-rankprop(obj=%s symbol=%s)" obj property)
  0)
(cl-defmethod occ-obj-rankprop ((obj  occ-tsk)
                                (property symbol))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  (occ-debug "occ-obj-rankprop(obj=%s symbol=%s)"
             obj
             property)
  0)
(cl-defmethod occ-obj-rankprop ((obj  occ-obj-ctx-tsk)
                                (property symbol))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  (occ-debug "occ-obj-rankprop(obj=%s symbol=%s)" obj property)
  (occ-obj-rankprop obj property))


(cl-defgeneric occ-obj-has-p (obj
                              property
                              value)
  "OBJ has property PROPERTY")
(cl-defmethod occ-obj-has-p ((obj occ-obj-tsk)
                             (prop symbol)
                             value)
  "OBJ has property PROPERTY"
  (let* ((tsk            (occ-obj-tsk obj))
         (tsk-prop-value (occ-obj-get-property tsk prop)))
    (occ-debug "occ-obj-has-p prop %s, (consp tsk-prop-value) %s" prop (consp tsk-prop-value))
    (occ-debug "occ-obj-has-p prop %s, (occ-obj-list-p prop) %s, value %s" prop (occ-obj-list-p prop) (prin1-to-string value))
    (occ-debug "occ-obj-has-p prop %s, (occ-obj-list-p prop) %s, tsk-prop-value %s" prop (occ-obj-list-p prop) (prin1-to-string tsk-prop-value))
    (if (occ-obj-list-p prop)
        (member value tsk-prop-value)
      (equal value tsk-prop-value))))


(cl-defgeneric occ-obj-get-property-value-from-ctx (obj
                                                    property)
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ.")
  ;; (occ-error "must return occ compatible value.")


(cl-defgeneric occ-obj-format-prop (obj
                                    property
                                    value)
  "Return format printable value of property PROPERTY."
  value)


(cl-defgeneric occ-obj-list-p (property)
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not.")

(cl-defmethod occ-obj-list-p ((property symbol))
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not."
  ;; 'list
  ;; (occ-error "Implement method occ-obj-list-p for property %s" property)
  (occ-debug "occ-obj-list-p: no method for property %s using default."
             property)
  nil)


(cl-defgeneric occ-prop-to-org (property
                                value)
  "Return string representation for property PROPERTY, Method
convert value VALUE of property PROPERTY from occ to org string
representation.")
(cl-defmethod occ-prop-to-org ((property symbol)
                               value)
  "Return string representation for property PROPERTY, Method
convert value VALUE of property PROPERTY from occ to org string
representation."
  ;; (occ-error "Implement method occ-prop-to-org for property %s" property)
  (occ-debug "occ-prop-to-org: no method for property %s using default."
             property)
  value)


(cl-defgeneric occ-prop-from-org (property
                                  value)
  "Return the Actual Object representation for property
PROPERTY, Method convert value VALUE of property PROPERTY from
org string to occ representation.")
(cl-defmethod occ-prop-from-org ((property symbol)
                                 value)
  "Return the Actual Object representation for property
PROPERTY, Method convert value VALUE of property PROPERTY from
org string to occ representation."
  ;; (occ-error "Implement method occ-prop-from-org for property %s" property)
  (occ-debug "occ-prop-from-org: no method for property %s using default." property)
  value)
(cl-defmethod occ-prop-from-org ((property symbol)
                                 (value string))
  "Return the Actual Object representation for property
PROPERTY, Method convert value VALUE of property PROPERTY from
org string to occ representation."
  ;; (occ-error "Implement method occ-prop-from-org for property %s" property)
  (occ-debug "occ-prop-from-org: no method for property %s using default." property)
  value)


(cl-defmethod occ-obj-readprop-from-user ((obj occ-obj-tsk)
                                          (property symbol))
  "Read value of list of elements if (occ-obj-list-p PROPERTY) else
element for property PROPERTY from user for OCC-TSK OBJ, must
return ORG compatible value."
  (occ-error "Implement method occ-obj-readprop-from-user for property %s" property))


(cl-defgeneric occ-obj-require-p (obj
                                  operation
                                  property
                                  values)
  "Used by OCC-OBJ-GEN-EDIT-IF-REQUIRED to decide for this property
_TEMPLATE_ if CALLABLE (helm method) should be generated."
  (occ-debug "occ-obj-require-p0 is called"))
(cl-defmethod occ-obj-require-p ((obj occ-obj-tsk)
                                 (operation (eql _operation_))
                                 (property  symbol)
                                 values)
  "Used by OCC-OBJ-GEN-EDIT-IF-REQUIRED to decide for this property
_TEMPLATE_ if CALLABLE (helm method) should be generated."
  (occ-debug "occ-obj-require-p1 is called")
  t)


(cl-defgeneric occ-obj-prop-default-value (obj
                                           property
                                           operation)
  "Return a default VALUE of property _TEMPLATE_.")
(cl-defmethod occ-obj-prop-default-value ((obj occ-obj-tsk)
                                          (property symbol)
                                          (operation symbol))
  "Return a default VALUE of property _TEMPLATE_."
  nil)
(cl-defmethod occ-obj-prop-default-value ((obj occ-obj-ctx-tsk)
                                          (property symbol)
                                          (operation symbol))
  "Return a default VALUE of property _TEMPLATE_."
  (occ-obj-get-property (occ-obj-ctx obj)
                    property))


(cl-defgeneric occ-obj-operation (obj
                                  operation
                                  property
                                  values)
  "Do the actual OPERATION.")
;; (cl-defmethod occ-obj-operation ((obj occ-obj-tsk)
;;                              (operation (eql XYZ))
;;                              (property      (eql x))
;;                              values)
;;   ())


(cl-defgeneric occ-do-checkout-prop (obj
                                     property)
  "Checkout property PROPERTY in case of force clock-in.")
(cl-defmethod occ-do-checkout-prop ((obj occ-obj-tsk)
                                    (property symbol))
  "Checkout property in case of force clock-in."
  (occ-error "Implement it for %s: Checkout property in case of force clock-in." property))

(occ-testing
 (cl-defmethod occ-obj-rankprop ((obj occ-tsk)
                                 (prop (eql _template_)))
   "Return the RANK (number) for OCC-TSK based on the property _TEMPLATE_")
 (cl-defmethod occ-obj-has-p ((obj occ-obj-tsk)
                              (property symbol)
                              value)
   "OBJ-has-property PROPERTY")
 (cl-defmethod occ-obj-get-property-value-from-ctx ((obj occ-ctx)
                                                    (property symbol))
   "Return occ compatible value of property PROPERTY from OCC-CTX OBJ."
   (occ-error "must return occ compatible value."))
 (cl-defmethod occ-obj-format-prop ((obj occ-obj-tsk)
                                    (property symbol)
                                    value)
   "Return format printable value of property PROPERTY."
   value)
 (cl-defmethod occ-obj-list-p ((prop (eql _template_)))
   "Is the property _TEMPLATE_ has VALUES in list, Method tell
   property represent list or not.")
 (cl-defmethod occ-obj-readprop-from-user ((obj occ-tsk)
                                           (prop (eql _template_)))
   "Read value of list of elements if (occ-obj-list-p PROPERTY) else
element for property PROPERTY from user for OCC-TSK OBJ, must
return ORG compatible value.")
 (cl-defmethod occ-obj-require-p ((obj occ-obj-tsk)
                                  (operation (eql _operation_))
                                  (prop (eql _template_))
                                  values)
   "Used by OCC-OBJ-GEN-EDIT-IF-REQUIRED to decide for this property
_TEMPLATE_ if CALLABLE (helm method) should be generated."
   (occ-debug "occ-obj-require-p3 is called"))
 (cl-defmethod occ-obj-prop-default-value ((obj occ-obj-tsk)
                                           (prop (eql _template_))
                                           (operation (eql _operation_)))
   "Return a default VALUE of property _TEMPLATE_.")
 (cl-defmethod occ-obj-operation ((obj occ-obj-tsk)
                                  (operation (eql _operation_))
                                  (prop (eql _template_))
                                  values)
   "Do the actual _OPERATION_.")
 (cl-defmethod occ-do-checkout-prop ((obj occ-obj-tsk)
                                     (prop (eql _template_)))
   "Checkout property _TEMPLATE_ in case of force clock-in."))

;;; occ-prop-intf.el ends here
