;;; occ-prop.el --- occ properties methods           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: sharad <>
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

;; https://www.gnu.org/software/emacs/manual/html_node/eieio/Quick-Start.html#Quick-Start
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Generic-Functions.html

;; The type specializer, (arg type), can specify one of the system types in the
;; following list. When a parent type is specified, an argument whose type is
;; any of its more specific child types, as well as grand-children,
;; grand-grand-children, etc. will also be compatible.
;;
;; integer (Parent type: number.)
;; number
;; null (Parent type: symbol.)
;; symbol
;; string (Parent type: array.)
;; array (Parent type: sequence.)
;; cons (Parent type: list.)
;; list (Parent type: sequence.)
;; marker
;; overlay
;; float (Parent type: number.)
;; window-configuration
;; process
;; window
;; subr
;; compiled-function
;; buffer
;; char-table (Parent type: array.)
;; bool-vector (Parent type: array.)
;; vector (Parent type: array.)
;; frame
;; hash-table
;; font-spec
;; font-entity
;; font-object


;;; Code:

(provide 'occ-prop-base)


(require 'dash)
;; (require 'subr)
(require 'org-misc-utils-lotus)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-cl-utils)
(require 'occ-obj-common)
(require 'occ-tree)
(eval-when-compile
  (require 'occ-macros))
(require 'occ-macros)
(require 'occ-obj-accessor)
(require 'occ-util-common)
(require 'occ-obj-method)
(require 'occ-rank)
(require 'occ-prop-intf)
(require 'occ-prop-org)
(require 'occ-assert)


(require 'ert)
(require 'ert-x)
(require 'el-mock)


;; TODO: multi-value property https://orgmode.org/manual/Using-the-property-API.html


(ert-deftest ert-occ-test-match-prop-method-args ()
  "Test"
  :expected-result :passed
  :tags '(occ)
  (should (equal (occ-obj-cl-method-sigs-matched-arg '(occ-obj-readprop-from-user     (`(occ-obj-ctx-tsk (eql ,val)) val))
                                                     '(occ-obj-get-property-value-from-ctx (`(occ-ctx (eql ,val)) val))
                                                     (occ-obj-make-ctx-at-point))
                 '(timebeing)))
  ;; do this test in buffer of a temporary file.
  (should (equal (occ-obj-cl-method-sigs-matched-arg '(occ-obj-readprop-from-user     (`(occ-obj-ctx-tsk (eql ,val)) val))
                                                     '(occ-obj-get-property-value-from-ctx (`(occ-ctx (eql ,val)) val))
                                                     (occ-obj-make-ctx-at-point))
                 '(timebeing root currfile))))


(cl-defgeneric occ-obj-properties-to-edit (obj)
  "return PROPERTIES list that can be edited.")
(cl-defgeneric occ-obj-properties-to-inherit (obj)
  "return PROPERTIES list that can be inherited.")
(cl-defgeneric occ-obj-properties-to-calculate-rank (obj)
  "return PROPERTIES list that can be used in calculating rank.")
(cl-defgeneric occ-obj-properties-to-checkout (obj)
  "return PROPERTIES list that can be checked-out.")


(cl-defmethod occ-obj-properties-to-edit ((class symbol))
  "return PROPERTIES list that can be edited."
  (occ-cl-method-param-values 'occ-obj-readprop-from-user
                              (list '\` `(,class (eql ,'(\, val))))
                              'val))

(cl-defmethod occ-obj-properties-to-edit ((obj occ-tsk))
  "return PROPERTIES list that can be edited."
  (occ-cl-collect-on-classes #'occ-obj-properties-to-edit
                             obj))

;; TODO: improve
(cl-defmethod occ-obj-properties-to-edit ((obj occ-obj-ctx-tsk))
  "return PROPERTIES list that can be edited."
  (occ-obj-cl-method-sigs-matched-arg '(occ-obj-readprop-from-user     (`(occ-obj-ctx-tsk (eql ,val)) val))
                                      '(occ-obj-get-property-value-from-ctx (`(occ-ctx (eql ,val)) val))
                                      (occ-obj-ctx obj)))


(cl-defmethod occ-obj-properties-to-inherit ((class symbol))
  "return PROPERTIES list that can be inherited."
  (occ-cl-method-param-values 'occ-obj-readprop-from-user
                              (list '\` `(,class (eql ,'(\, val))))
                              'val))

(cl-defmethod occ-obj-properties-to-inherit ((obj occ-obj-tsk))
  "return PROPERTIES list that can be inherited."
  (occ-cl-collect-on-classes #'occ-obj-properties-to-inherit
                             obj))


(defun occ-readprop-props () ;;TODO: check about them
  "return PROPERTIES list that can be inherited."
  (occ-obj-properties-to-inherit 'occ-obj-ctx-tsk))


(cl-defmethod occ-obj-properties-to-calculate-rank ((class symbol))
  "return PROPERTIES list that can be used in calculating rank."
  (occ-cl-method-param-values 'occ-obj-rankprop
                              (list '\` `(,class (eql ,'(\, val))))
                              'val))

(cl-defmethod occ-obj-properties-to-calculate-rank ((obj occ-obj-tsk))
  "return PROPERTIES list that can be used in calculating rank."
  (occ-cl-collect-on-classes #'occ-obj-properties-to-calculate-rank ;; occ-properties-to-calcuate-rank
                             obj))


(cl-defmethod occ-obj-properties-to-checkout ((class symbol))
  "return PROPERTIES list that can be checked-out."
  (occ-cl-method-param-values 'occ-do-checkout-prop ;NOTE: user have to define them for each properties.
                              (list '\` `(,class (eql ,'(\, val))))
                              'val))

(cl-defmethod occ-obj-properties-to-checkout ((obj occ-obj-tsk))
  "return PROPERTIES list that can be checked-out."
  (occ-cl-collect-on-classes #'occ-obj-properties-to-checkout
                             obj))


(defun occ-internal-remove-template-symbol (prop-list)
  (cl-remove-if #'(lambda (prop)
                    (string-match "^_.+_$" (symbol-name prop)))
                prop-list))


;; TODO: occ-prop-base.el: Warning: ‘cl-next-method-p’ is an obsolete macro (as of 25.1); make sure there’s always a next method, or catch ‘cl-no-next-method’ instead [7 times]
(cl-defmethod occ-obj-properties-to-edit :around (obj)
  "return PROPERTIES list that can be edited."
  (ignore obj)
  (condition-case e ;; if (cl-next-method-p)
      (occ-internal-remove-template-symbol (cl-call-next-method))
    ((cl-no-next-method) (occ-error "No
(cl-defmethod occ-obj-properties-to-edit (obj)
  ...)

method provided."))))
(cl-defmethod occ-obj-properties-to-inherit :around (obj)
  "return PROPERTIES list that can be inherited."
  (ignore obj)
  (condition-case e ;; if (cl-next-method-p)
      (occ-internal-remove-template-symbol (cl-call-next-method))
    ((cl-no-next-method) (occ-error "No
(cl-defmethod occ-obj-properties-to-inherit (obj)
   ...)

method provided."))))
(cl-defmethod occ-obj-properties-to-calculate-rank :around (obj)
  "return PROPERTIES list that can be used in calculating rank."
  (ignore obj)
  (condition-case e ;; if (cl-next-method-p)
      (occ-internal-remove-template-symbol (cl-call-next-method))
    ((cl-no-next-method) (occ-error "No
(cl-defmethod occ-obj-properties-to-calculate-rank (obj)
  ...)

method provided."))))
(cl-defmethod occ-obj-properties-to-checkout :around (obj)
  "return PROPERTIES list that can be checked-out."
  (ignore obj)
  (condition-case e ;; if (cl-next-method-p)
      (occ-internal-remove-template-symbol (cl-call-next-method))
    ((cl-no-next-method) (occ-error "No
(cl-defmethod occ-obj-properties-to-checkout (obj)
  ...)

method provided."))))


;; NOTE: These two around methods not belongs to occ-prop-intf.el
;;       they belongs here only.
(cl-defmethod occ-obj-readprop-from-user :around ((obj  occ-obj-tsk)
                                                  (prop symbol))
  "Read value of element of list for property PROP from user for
OCC-TSK OBJ."
  (ignore obj)
  (condition-case e ;; if (cl-next-method-p)
      (cl-call-next-method)
    ((cl-no-next-method) (occ-error "No
(cl-defmethod occ-obj-readprop-from-user ((obj occ-obj-tsk) (prop (eql %s)))
  ...)

method provided."
               prop))))

(cl-defmethod occ-obj-readprop-from-user :around ((obj  occ-obj-tsk)
                                                  (prop symbol))
  "Read value of element of list for property PROP from user for
OCC-TSK OBJ."
  (ignore obj)
  (condition-case e ;; if (cl-next-method-p)
      (cl-call-next-method)
    ((cl-no-next-method) (occ-error "No
(cl-defmethod occ-obj-readprop-from-user ((obj occ-obj-tsk) (prop (eql %s)))
   ...)

method provided."
               prop))))


(cl-defmethod occ-obj-rereadprop-value ((prop symbol)
                                        value)
  "Read org string property PROP to occ representation."
  (occ-assert (not (consp value)))
  (if (occ-obj-list-p prop)
      (let* ((values (and value (split-string value))))
        (mapcar #'(lambda (v)
                    ;; from Org world to Occ world
                    (occ-obj-prop-from-org prop
                                           v))
                (mapcar #'org-entry-restore-space
                        values)))
    (occ-obj-prop-from-org prop
                           value)))

(cl-defmethod occ-obj-reread-props ((obj occ-tsk))
  "Read all org string properties for task TSK to occ representation."
  (let ((props-by-is-list   (occ-cl-method-param-case '(occ-obj-list-p (`((eql ,val)) val))))
        (props-by-converter (occ-cl-method-param-case '(occ-obj-prop-from-org (`((eql ,val) t) val)))))
    (let ((props (-union props-by-is-list
                         props-by-converter))) ;dash
      (dolist (p props)
        (let* ((value         (occ-obj-get-property obj p))
               (rearead-value (occ-obj-rereadprop-value p value)))
          (occ-obj-set-property obj p rearead-value))))))

(cl-defmethod occ-obj-reread-props :around (obj)
  "return PROPERTIES list that can be checked-out."
  (ignore obj)
  (condition-case e ;; if (cl-next-method-p)
      (occ-internal-remove-template-symbol (cl-call-next-method))
    ((cl-no-next-method) (occ-error "No
(cl-defmethod occ-obj-reread-props (obj)
  ...)

method provided."))))


(cl-defmethod occ-obj-require-p ((obj       occ-obj-tsk)
                                 (operation (eql get))
                                 (prop      symbol)
                                 values)
  (ignore obj)
  (occ-debug "occ-obj-require-p11 prop %s operation %s values %s is called" prop operation values)
  nil)

(cl-defmethod occ-obj-require-p ((obj       occ-obj-tsk)
                                 (operation (eql add))
                                 (prop      symbol)
                                 values)
  (occ-debug "occ-obj-require-p7 prop %s operation %s values %s is called" prop operation values)
  (not (occ-obj-has-p obj prop
                      values)))

(cl-defmethod occ-obj-require-p ((obj       occ-obj-tsk)
                                 (operation (eql put))
                                 (prop      symbol)
                                 values)
  (ignore obj)
  (occ-debug "occ-obj-require-p10 prop %s operation %s values %s is called" prop operation values)
  nil)

(cl-defmethod occ-obj-require-p ((obj       occ-obj-tsk)
                                 (operation (eql remove))
                                 (prop      symbol)
                                 values)
  (occ-debug "occ-obj-require-p8 prop %s operation %s values %s is called" prop operation values)
  (occ-obj-has-p obj prop
                 values))

(cl-defmethod occ-obj-require-p ((obj       occ-obj-tsk)
                                 (operation (eql member))
                                 (prop      symbol)
                                 values)
  (ignore obj)
  (occ-debug "occ-obj-require-p9 prop %s operation %s values %s is called" prop operation values)
  nil)


(cl-defmethod occ-obj-valid-p ((operation symbol)
                               (prop      symbol))
  (ignore prop)
  (memq operation
        '(add remove get put member)))


(cl-defmethod occ-obj-operations-for-prop ((class symbol)
                                           (prop  symbol))
  ;; check about (occ-obj-list-p prop) also
  (let ((ops (append (occ-cl-method-param-values 'occ-obj-operation
                                                 (list '\` `(,class (eql ,'(\, val)) symbol t))
                                                 'val)
                     (occ-cl-method-param-values 'occ-obj-operation
                                                 (list '\` `(,class (eql ,'(\, val)) (eql ,prop) t))
                                                 'val))))
    (delete-dups ops)))

(cl-defmethod occ-obj-operations-for-prop ((obj  occ-obj-tsk)
                                           (prop symbol))
  ;; check about (occ-obj-list-p prop) also
  (let ((ops (occ-cl-collect-on-classes #'(lambda (class)
                                            (occ-obj-operations-for-prop class
                                                                         prop))
                                    obj)))
    (delete-dups ops)))

(occ-testing
 (occ-obj-operations-for-prop 'occ-obj-tsk 'root))


;; TODO: Implement Plist with title here (??)


;;; occ-prop.el ends here
