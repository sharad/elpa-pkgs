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


(occ-testing
 (cl-defmethod occ-rankprop ((obj occ-tsk)
                             (prop (eql _template_)))
   "Return the RANK (number) for OCC-TSK based on the property _TEMPLATE_")
 (cl-defmethod occ-has-p ((obj occ-obj-tsk)
                          (property symbol)
                          value)
   "OBJ-has-property PROPERTY")
 (cl-defmethod occ-get-property-value-from-ctx ((obj occ-ctx)
                                                (property symbol))
   "Return occ compatible value of property PROPERTY from OCC-CTX OBJ."
   (occ-error "must return occ compatible value."))
 (cl-defmethod occ-format-prop ((obj occ-obj-tsk)
                                (property symbol)
                                value)
   "Return format printable value of property PROPERTY."
   value)
 (cl-defmethod occ-list-p ((prop (eql _template_)))
   "Is the property _TEMPLATE_ has VALUES in list, Method tell
   property represent list or not.")
 (cl-defmethod occ-prop-elem-to-org   ((prop (eql _template_))
                                       value)
   "Return string representation for property _TEMPLATE_, Method
convert value VALUE of property PROPERTY from occ to org string
representation.")
 (cl-defmethod occ-prop-elem-from-org ((prop (eql _template_))
                                       value)
   "Return the Actual Object representation for property
_TEMPLATE_, Method convert value VALUE of property PROPERTY from
org string to occ representation.")
 (cl-defmethod occ-readprop-elem-from-user ((obj occ-tsk)
                                            (prop (eql _template_)))
   "READ the value for property _TEMPLATE_, Read value of element
of list for property PROPERTY from user for OCC-TSK OBJ, must
return ORG compatible value.")
 (cl-defmethod occ-readprop-from-user ((obj occ-tsk)
                                       (prop (eql _template_)))
   "Read value of list of elements if (occ-list-p PROPERTY) else
element for property PROPERTY from user for OCC-TSK OBJ, must
return ORG compatible value.")
 (cl-defmethod occ-require-p ((obj occ-obj-tsk)
                              (operation (eql _operation_))
                              (prop (eql _template_))
                              values)
   "Used by OCC-GEN-EDIT-IF-REQUIRED to decide for this property
_TEMPLATE_ if CALLABLE (helm method) should be generated.")
 (cl-defmethod occ-prop-default-value ((obj occ-obj-tsk)
                                       (prop (eql _template_))
                                       (operation (eql _operation_)))
   "Return a default VALUE of property _TEMPLATE_.")
 (cl-defmethod occ-operation ((obj occ-obj-tsk)
                              (operation (eql _operation_))
                              (prop (eql _template_))
                              values)
   "Do the actual _OPERATION_.")
 (cl-defmethod occ-checkout-prop ((obj occ-obj-tsk)
                                  (prop (eql _template_)))
   "Checkout property _TEMPLATE_ in case of force clock-in."))


(cl-defgeneric occ-rankprop (obj
                             property)
  "Return the RANK (number) for OBJ based on the property PROPERTY")

(cl-defmethod occ-rankprop (obj
                            property)
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  ;; too much output
  ;; (occ-debug :debug "occ-rank(tsk-pair=%s ctx=%s)" tsk-pair ctx)
  (occ-debug :debug "occ-rankprop(obj=%s symbol=%s)" obj property)
  0)

(cl-defmethod occ-rankprop ((obj  occ-tsk)
                            (property symbol))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  (occ-debug :debug "occ-rankprop(obj=%s symbol=%s)"
             obj
             property)
  0)

(cl-defmethod occ-rankprop ((obj  occ-obj-ctx-tsk)
                            (property symbol))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  (occ-debug :debug "occ-rankprop(obj=%s symbol=%s)" obj property)
  (occ-rankprop obj property))


(cl-defgeneric occ-has-p (obj
                          property
                          value)
  "OBJ-has-property PROPERTY")
(cl-defmethod occ-has-p ((obj occ-obj-tsk)
                         (prop symbol)
                         value)
  (let ((tsk (occ-obj-tsk obj)))
    (if (occ-list-p prop)
        (memq value
              (occ-get-property-value-from-ctx tsk
                                               prop))
      (equal value
             (occ-get-property-value-from-ctx tsk
                                              prop)))))


(cl-defgeneric occ-get-property-value-from-ctx (obj
                                                property)
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ."
  (occ-error "must return occ compatible value."))


(cl-defgeneric occ-format-prop (obj
                                property
                                value)
  "Return format printable value of property PROPERTY."
  value)


(cl-defgeneric occ-list-p (property)
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not.")

(cl-defmethod occ-list-p ((property symbol))
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not."
  ;; 'list
  ;; (occ-error "Implement method occ-list-p for property %s" property)
  (occ-debug :debug "occ-list-p: no method for property %s using default."
             property)
  nil)


(cl-defgeneric occ-prop-elem-to-org (property
                                     value)
  "Return string representation for property PROPERTY, Method
convert value VALUE of property PROPERTY from occ to org string
representation.")

(cl-defmethod occ-prop-elem-to-org ((property symbol)
                                    value)
  "Return string representation for property PROPERTY, Method
convert value VALUE of property PROPERTY from occ to org string
representation."
  ;; (occ-error "Implement method occ-prop-elem-to-org for property %s" property)
  (occ-debug :debug "occ-prop-elem-to-org: no method for property %s using default."
             property)
  value)


(cl-defgeneric occ-prop-to-org (property
                                value)
  "Return string representation for list of elements
if (occ-list-p PROPERTY) else element, Method convert value VALUE
of property PROPERTY from occ to org string representation.")

(cl-defmethod occ-prop-to-org ((property symbol)
                               value)
  "Return string representation for list of elements concatenated with ','
if (occ-list-p PROPERTY) else element, Method convert value VALUE
of property PROPERTY from occ to org string representation."
  (occ-debug :debug "occ-prop-to-org: no method for property %s using default."
             property)
  (occ-debug :debug "occ-prop-to-org: no method for prop %s using default." prop)
  (occ-message "occ-prop-from-org: no method for prop %s using default." prop)
  (occ-message "occ-prop-from-org: no method for values %s." values)
  (if (occ-list-p property)
      (let ((value-list (mapcar #'(lambda (v)
                                    (occ-prop-elem-to-org prop
                                                          v))
                                value)))
        (string-join value-list ","))
    (let ((retval (ignore-error (occ-prop-elem-to-org property
                                                      value))))
      (if retval
          retval
        value))))


(cl-defgeneric occ-prop-elem-from-org (property
                                       value)
  "Return the Actual Object representation for property
PROPERTY, Method convert value VALUE of property PROPERTY from
org string to occ representation.")
(cl-defmethod occ-prop-elem-from-org ((property symbol)
                                      (value string))
  "Return the Actual Object representation for property
PROPERTY, Method convert value VALUE of property PROPERTY from
org string to occ representation."
  ;; (occ-error "Implement method occ-prop-elem-from-org for property %s" property)
  (occ-debug :debug
             "occ-prop-elem-from-org: no method for property %s using default." property)
  value)


(cl-defgeneric occ-prop-from-org (property
                                  value)
  "Return the Actual Object representation for list of elements
if (occ-list-p PROPERTY) else element PROPERTY, Method convert
value VALUE of property PROPERTY from org string to occ
representation. Property specific method not required to be
define by user.")

(cl-defmethod occ-prop-from-org ((property symbol)
                                 (value string))
  "Return the Actual Object representation for list of elements
if (occ-list-p PROPERTY) else element PROPERTY, Method convert
value VALUE of property PROPERTY from org string to occ
representation. Property specific method not required to be
define by user."
  ;; (occ-error "Implement method occ-prop-elem-from-org for property %s" property)
  (occ-debug :debug
             "occ-prop-elem-from-org: no method for property %s using default." property)
  (if (occ-list-p property)
      (mapcar #'(lambda (v)
                  (occ-prop-elem-from-org prop
                                          v))
              value)
    (let ((retval (ignore-error (occ-prop-elem-from-org property
                                                        value))))
      (if retval
          retval
        value))))


(cl-defgeneric occ-readprop-elem-from-user (obj
                                            property)
  "READ the value for property PROPERTY, Read value of element
of list for property PROPERTY from user for OCC-TSK OBJ, must
return ORG compatible value.")
;; TODO: should not we make them to be converted to OCC value here.
(cl-defmethod occ-readprop-elem-from-user ((obj occ-obj-tsk)
                                           (property symbol))
  "READ the value for property PROPERTY, Read value of element
of list for property PROPERTY from user for OCC-TSK OBJ, must
return ORG compatible value."
  (occ-error "Implement method occ-readprop-elem-from-user for property %s " property))


(cl-defmethod occ-readprop-from-user ((obj occ-obj-tsk)
                                      (property symbol))
  "Read value of list of elements if (occ-list-p PROPERTY) else
element for property PROPERTY from user for OCC-TSK OBJ, must
return ORG compatible value."
  (if (occ-list-p property)
      ;;try with occ-readprop-elem-from-user in below method
      (occ-prop-util-readprop-list-from-user obj property)
    ;; if occ-readprop-elem-from-user define then try it
    ;; (let ((retval (ignore-error (occ-readprop-elem-from-user obj property))))
    ;;   (if retval
    ;;       retval
    ;;     (occ-error "Implement method occ-readprop-from-user for property %s" property)))
    (occ-error "Implement method occ-readprop-from-user for property %s" property)))


(cl-defgeneric occ-require-p (obj
                              operation
                              property
                              values)
  "Used by OCC-GEN-EDIT-IF-REQUIRED to decide for this property
_TEMPLATE_ if CALLABLE (helm method) should be generated.")

(cl-defmethod occ-require-p ((obj occ-obj-tsk)
                             (operation (eql _operation_))
                             (property  symbol)
                             values)
  "Used by OCC-GEN-EDIT-IF-REQUIRED to decide for this property
_TEMPLATE_ if CALLABLE (helm method) should be generated."
  t)


(cl-defgeneric occ-prop-default-value (obj
                                       property
                                       operation)
  "Return a default VALUE of property _TEMPLATE_.")

(cl-defmethod occ-prop-default-value ((obj occ-obj-tsk)
                                      (property symbol)
                                      (operation symbol))
  "Return a default VALUE of property _TEMPLATE_."
  nil)

(cl-defmethod occ-prop-default-value ((obj occ-obj-ctx-tsk)
                                      (property symbol)
                                      (operation symbol))
  "Return a default VALUE of property _TEMPLATE_."
  (occ-get-property-value-from-ctx (occ-obj-ctx obj)
                                   property))


(cl-defgeneric occ-operation (obj
                              operation
                              property
                              values)
  "Do the actual OPERATION.")

;; (cl-defmethod occ-operation ((obj occ-obj-tsk)
;;                              (operation (eql XYZ))
;;                              (property      (eql x))
;;                              values)
;;   ())


(cl-defgeneric occ-checkout-prop (obj
                                  property)
  "Checkout property PROPERTY in case of force clock-in.")

(cl-defmethod occ-checkout-prop ((obj occ-obj-tsk)
                                 (property symbol))
  "Checkout property in case of force clock-in."
  (occ-error "Implement it for %s: Checkout property in case of force clock-in." property))

;;; occ-prop-intf.el ends here
