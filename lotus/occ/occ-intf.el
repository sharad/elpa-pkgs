;;; occ-intf.el --- occ intf                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <spratap@merunetworks.com>
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

(provide 'occ-intf)


(require 'occ-impl)

;; (cl-defgeneric occ-obj-intf-rankprop (obj
(cl-defgeneric occ-obj-intf-rank (obj
                                  property)
  "Return the RANK (number) for OBJ based on the property PROPERTY")
(cl-defmethod occ-obj-intf-rank (obj
                                 property)
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  ;; too much output
  ;; (occ-debug "occ-obj-rank(tsk-pair=%s ctx=%s)" tsk-pair ctx)
  (occ-debug "occ-obj-rankprop(obj=%s symbol=%s)" obj property)
  (occ-obj-impl-rank obj property))
(cl-defmethod occ-obj-intf-rank ((obj  occ-tsk)
                                 (property symbol))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  (occ-debug "occ-obj-rankprop(obj=%s symbol=%s)"
             obj
             property)
  (occ-obj-impl-rank obj property))
(cl-defmethod occ-obj-intf-rank ((obj  occ-obj-ctx-tsk)
                                 (property symbol))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  (occ-debug "occ-obj-rankprop(obj=%s symbol=%s)" obj property)
  (occ-obj-impl-rank obj property))


;; adding base propetry NIL for helping to add relative properties
(cl-defmethod occ-obj-intf-rank ((obj  occ-tsk)
                                 (property (eql nil)))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  (occ-debug "occ-obj-rankprop(obj=%s symbol=%s)"
             obj
             property)
  (occ-obj-impl-rank obj property))
(cl-defmethod occ-obj-intf-rank ((obj  occ-obj-ctx-tsk)
                                 (property (eql nil)))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  (occ-debug "occ-obj-rankprop(obj=%s symbol=%s)" obj property)
  (occ-obj-impl-rank obj property))


;; (cl-defgeneric occ-obj-intf-has-p (obj
(cl-defgeneric occ-obj-intf-has-p (obj
                                   property
                                   value)
  "OBJ has property VALUE for PROPERTY")
(cl-defmethod occ-obj-intf-has-p ((obj occ-obj-tsk)
                                  (prop symbol)
                                  value)
  "OBJ has property VALUE for PROPERTY"
  (occ-obj-impl-has-p obj prop value))


;; (cl-defgeneric occ-obj-intf-get-property-value-from-ctx (obj
(cl-defgeneric occ-obj-intf-get-property-value-from-ctx (obj
                                                         property)
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ.")

(cl-defgeneric occ-obj-intf-get (ctx
                                 property
                                 obj)
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ.")
  ;; (occ-error "must return occ compatible value.")


;; (cl-defgeneric occ-obj-intf-format-prop (obj
(cl-defgeneric occ-obj-intf-format (obj
                                    property
                                    value)
  "Return format printable value of property PROPERTY."
  (ignore obj)
  (ignore property)
  value)


;; (cl-defgeneric occ-obj-intf-list-p (property)
(cl-defgeneric occ-obj-intf-list-p (property)
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not.")

(cl-defmethod occ-obj-intf-list-p ((property symbol))
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not."
  ;; 'list
  ;; (occ-error "Implement method occ-obj-intf-list-p for property %s" property)
  (occ-debug "occ-obj-list-p: no method for property %s using default."
             property)
  (occ-obj-impl-list-p property))


;; (cl-defgeneric occ-obj-intf-prop-to-org (property
(cl-defgeneric occ-obj-intf-to-org (property
                                    value)
  "Return string representation for property PROPERTY, Method
convert value VALUE of property PROPERTY from occ to org string
representation.")
;; (cl-defmethod occ-obj-intf-to-org ((property symbol)
(cl-defmethod occ-obj-intf-to-org ((property symbol)
                                   value)
  "Return string representation for property PROPERTY, Method
convert value VALUE of property PROPERTY from occ to org string
representation."
  ;; (occ-error "Implement method occ-obj-intf-to-org for property %s" property)
  (occ-debug "occ-obj-prop-to-org: no method for property %s using default."
             property)
  (occ-obj-impl-to-org property value))


;; (cl-defgeneric occ-obj-intf-prop-from-org (property
(cl-defgeneric occ-obj-intf-from-org (property
                                      value)
  "Return the Actual Object representation for property
PROPERTY, Method convert value VALUE of property PROPERTY from
org string to occ representation.")
;; (cl-defmethod occ-obj-intf-from-org ((property symbol)
(cl-defmethod occ-obj-intf-from-org ((property symbol)
                                     value)
  "Return the Actual Object representation for property
PROPERTY, Method convert value VALUE of property PROPERTY from
org string to occ representation."
  ;; (occ-error "Implement method occ-obj-intf-from-org for property %s" property)
  (occ-debug "occ-obj-prop-from-org: no method for property %s using default." property)
  (occ-obj-impl-prop-from-org property value))
;; (cl-defmethod occ-obj-intf-from-org ((property symbol)
(cl-defmethod occ-obj-intf-from-org ((property symbol)
                                     (value string))
  "Return the Actual Object representation for property
PROPERTY, Method convert value VALUE of property PROPERTY from
org string to occ representation."
  ;; (occ-error "Implement method occ-obj-intf-from-org for property %s" property)
  (occ-debug "occ-obj-prop-from-org: no method for property %s using default." property)
  (occ-obj-impl-from-org property value))


;; (cl-defmethod occ-obj-intf-readprop-from-user ((obj occ-obj-tsk)
(cl-defmethod occ-obj-intf-readprop-from-user ((obj occ-obj-tsk)
                                               (property symbol))
  "Read value of list of elements if (occ-obj-list-p PROPERTY) else
element for property PROPERTY from user for OCC-TSK OBJ, must
return ORG compatible value."
  (ignore obj)
  (occ-error "Implement method occ-obj-intf-readprop-from-user for property %s" property))

(cl-defmethod occ-obj-intf-get ((user occ-obj-user-agent)
                                (property symbol)
                                (ctsk occ-obj-tsk))
  "Read value of list of elements if (occ-obj-list-p PROPERTY) else
element for property PROPERTY from user for OCC-TSK OBJ, must
return ORG compatible value."
  (ignore obj)
  (occ-obj-intf-get user property ctsk))


;; (cl-defgeneric occ-obj-intf-require-p (obj
(cl-defgeneric occ-obj-intf-require-p (obj
                                       operation
                                       property
                                       values)
  "Used by OCC-OBJ-INTF-GEN-EDIT-IF-REQUIRED to decide for this property
_TEMPLATE_ if CALLABLE (helm method) should be generated."
  (ignore obj)
  (ignore operation)
  (ignore property)
  (ignore values)
  (occ-debug "occ-obj-require-p0 is called"))
(cl-defmethod occ-obj-intf-require-p ((obj occ-obj-tsk)
                                      (operation (eql _operation_))
                                      (property  symbol)
                                      values)
  "Used by OCC-OBJ-INTF-GEN-EDIT-IF-REQUIRED to decide for this property
_TEMPLATE_ if CALLABLE (helm method) should be generated."
  (occ-obj-impl-require-p obj operation property values))


;; (cl-defgeneric occ-obj-intf-prop-default-value (obj
(cl-defgeneric occ-obj-intf-default (obj
                                     property
                                     operation)
  "Return a default VALUE of property _TEMPLATE_.")
(cl-defmethod occ-obj-intf-default ((obj occ-obj-tsk)
                                    (property symbol)
                                    (operation symbol))
  "Return a default VALUE of property _TEMPLATE_."
  (ignore obj)
  (ignore property)
  (ignore operation)
  nil)
(cl-defmethod occ-obj-intf-default ((obj occ-obj-ctx-tsk)
                                    (property symbol)
                                    (operation symbol))
  "Return a default VALUE of property _TEMPLATE_."
  (occ-obj-impl-default obj property operation))


;; (cl-defgeneric occ-obj-intf-operation (obj
(cl-defgeneric occ-obj-intf-operation (obj
                                       operation
                                       property
                                       values)
  "Do the actual OPERATION.")
(cl-defmethod occ-obj-intf-operation (obj
                                      operation
                                      property
                                      values)
  "Do the actual OPERATION."
  (occ-obj-impl-operation obj operation property values))
;; (cl-defmethod occ-obj-intf-operation ((obj occ-obj-tsk)
;;                              (operation (eql XYZ))
;;                              (property      (eql x))
;;                              values)
;;   ())

(cl-defgeneric occ-do-intf-operation (obj
                                      operation
                                      property
                                      values)
  "Do the actual OPERATION.")
(cl-defmethod occ-do-intf-operation (obj
                                     operation
                                     property
                                     values)
  "Do the actual OPERATION."
  (occ-do-impl-operation obj operation property values))


;; (cl-defgeneric occ-do-intf-checkout-prop (obj
(cl-defgeneric occ-do-intf-checkout (obj
                                     property)
  "Checkout property PROPERTY in case of force clock-in.")
(cl-defmethod occ-do-intf-checkout ((obj occ-obj-tsk)
                                    (property symbol))
  "Checkout property in case of force clock-in."
  (occ-do-impl-checkout obj property))




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

;;; occ-intf.el ends here
