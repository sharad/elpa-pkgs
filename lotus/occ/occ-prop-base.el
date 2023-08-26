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
(require 'occ-intf)
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
  (should (equal (occ-obj-cl-method-sigs-matched-arg '(occ-obj-impl-get (`(occ-user-agent (eql ,val) occ-obj-ctx-tsk) val))
                                                     '(occ-obj-impl-get (`(occ-ctx (eql ,val) null) val))
                                                     (occ-obj-make-ctx-at-point))
                 '(timebeing)))
  ;; do this test in buffer of a temporary file.
  (should (equal (occ-obj-cl-method-sigs-matched-arg '(occ-obj-impl-get (`(occ-user-agent (eql ,val) occ-obj-ctx-tsk) val))
                                                     '(occ-obj-impl-get (`(occ-ctx (eql ,val) null) val))
                                                     `(,(occ-obj-make-ctx-at-point) val nil))
                 '(timebeing root currfile))))


;; (cl-defmethod occ-obj-cl-method-sigs-matched-arg ((method-sig1 cons)
;;                                                   (method-sig2 cons)
;;                                                   (args cons))
;;   (let ((slots (occ-cl-method-param-case-with-value-new method-sig2 args)))
;;     (cl-remove-if-not #'(lambda (arg) (memq arg slots))
;;                       (occ-cl-method-param-case method-sig1))))

;; (occ-cl-method-param-case-with-value-new  '(occ-obj-impl-get (`(occ-ctx (eql ,val) null) val)) '((occ-obj-make-ctx-at-point) val nil))


;; (occ-cl-method-param-signs 'occ-obj-impl-get)
;; ((occ-user-agent symbol occ-obj-tsk) (occ-ctx (eql git-branch) null) (occ-user-agent (eql timebeing) occ-tsk) (occ-user-agent (eql root) occ-obj-ctx-tsk) (occ-ctx (eql root) null) (occ-user-agent (eql currfile) occ-obj-ctx-tsk) (occ-ctx (eql currfile) null) (occ-user-agent symbol occ-obj-tsk))



(cl-defgeneric occ-do-add-ineq (property
                                ineq)
  "Add ineq for property PROPERTY.")
(cl-defmethod occ-do-add-ineq ((property symbol)
                               (ineq list))
  "Add ineq for property PROPERTY."
  (occ-do-add-ineq-internal property ineq))
(cl-defmethod occ-do-add-ineq ((property symbol)
                               (ineq string))
  "Add ineq for property PROPERTY."
  (occ-do-add-ineq-internal property
                            ineq))

(cl-defgeneric occ-obj-ineq (property)
  "Add ineq for property PROPERTY.")
(cl-defmethod occ-obj-ineq ((property symbol))
  "Add ineq for property PROPERTY."
  (occ-obj-ineq-internal property))

(cl-defgeneric occ-obj-priority (property)
  "Add ineq for property PROPERTY.")
(cl-defmethod occ-obj-priority ((property symbol))
  "Add ineq for property PROPERTY."
  (occ-obj-priority-internal property))


(cl-defgeneric occ-obj-priority-rank (obj prop)
  "Get prioritised rank.")

(cl-defmethod occ-obj-priority-rank ((obj number)
                                     (prop symbol))
  (let ((value obj)
        (priority (occ-obj-priority prop)))
    (unless priority
      (occ-error "Priority is not present for property %s" prop))
    (* priority
       value)))

(cl-defmethod occ-obj-priority-rank ((obj occ-tsk)
                                     (prop symbol))
  "Get prioritised rank."
  (occ-obj-priority-rank (occ-obj-intf-rank obj prop)
                         prop))


(cl-defmethod occ-obj-priority-rank ((obj  occ-obj-ctx-tsk)
                                     (prop symbol))
  "Get prioritised rank."
  (occ-obj-priority-rank (occ-obj-intf-rank obj prop)
                         prop))


(cl-defgeneric occ-obj-properties-to-edit (obj)
  "return PROPERTIES list that can be edited.")
(cl-defgeneric occ-obj-properties-to-inherit (obj)
  "return PROPERTIES list that can be inherited.")
(cl-defgeneric occ-obj-properties-to-calculate-rank (obj)
  "return PROPERTIES list that can be used in calculating rank.")
(cl-defgeneric occ-obj-properties-to-checkout (obj)
  "return PROPERTIES list that can be checked-out.")


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

method provided.")
               prop)))

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

method provided.")
               prop)))


(cl-defmethod occ-obj-properties-to-edit ((class symbol))
  "return PROPERTIES list that can be edited."
  (occ-cl-method-param-values 'occ-obj-impl-get
                              (list '\` `(occ-user-agent (eql ,'(\, val)) ,class))
                              'val))

(cl-defmethod occ-obj-properties-to-edit ((obj occ-tsk))
  "return PROPERTIES list that can be edited."
  (occ-cl-collect-on-classes #'occ-obj-properties-to-edit
                             obj))

;; TODO: improve
(cl-defmethod occ-obj-properties-to-edit ((obj occ-obj-ctx-tsk))
  "return PROPERTIES list that can be edited."
  (occ-obj-cl-method-sigs-matched-arg '(occ-obj-impl-get     (`(occ-user-agent (eql ,val) occ-obj-ctx-tsk) val))
                                      '(occ-obj-impl-get     (`(occ-ctx (eql ,val) null) val))
                                      `(,(occ-obj-ctx obj) val nil)))


(cl-defmethod occ-obj-properties-to-inherit ((class symbol))
  "return PROPERTIES list that can be inherited."
  (occ-cl-method-param-values 'occ-obj-impl-get
                              (list '\` `(occ-user-agent (eql ,'(\, val)) ,class))
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
  (occ-cl-method-param-values 'occ-obj-impl-rank
                              (list '\` `(,class (eql ,'(\, val))))
                              'val))

(cl-defmethod occ-obj-properties-to-calculate-rank ((obj occ-obj-tsk))
  "return PROPERTIES list that can be used in calculating rank."
  (occ-cl-collect-on-classes #'occ-obj-properties-to-calculate-rank ;; occ-properties-to-calcuate-rank
                             obj))


(cl-defmethod occ-obj-properties-to-checkout ((class symbol))
  "return PROPERTIES list that can be checked-out."
  (occ-cl-method-param-values 'occ-do-impl-checkout ;NOTE: user have to define them for each properties.
                              (list '\` `(,class (eql ,'(\, val))))
                              'val))

(cl-defmethod occ-obj-properties-to-checkout ((obj occ-obj-tsk))
  "return PROPERTIES list that can be checked-out."
  (occ-cl-collect-on-classes #'occ-obj-properties-to-checkout
                             obj))


(cl-defmethod occ-obj-operations-for-prop ((class symbol)
                                           (prop  symbol))
  ;; check about (occ-obj-intf-list-p prop) also
  (let ((ops (append (occ-cl-method-param-values 'occ-do-impl-operation
                                                 (list '\` `(,class (eql ,'(\, val)) symbol t))
                                                 'val)
                     (occ-cl-method-param-values 'occ-do-impl-operation
                                                 (list '\` `(,class (eql ,'(\, val)) (eql ,prop) t))
                                                 'val))))
    (delete-dups ops)))

(cl-defmethod occ-obj-operations-for-prop ((obj  occ-obj-tsk)
                                           (prop symbol))
  ;; check about (occ-obj-intf-list-p prop) also
  (let ((ops (occ-cl-collect-on-classes #'(lambda (class)
                                            (occ-obj-operations-for-prop class
                                                                         prop))
                                    obj)))
    (delete-dups ops)))

(occ-testing
 (occ-obj-operations-for-prop 'occ-obj-tsk 'root))


(defun occ-internal-remove-template-symbol (prop-list)
  (cl-remove-if #'(lambda (prop)
                    (string-match "^_.+_$" (symbol-name prop)))
                prop-list))


(cl-defmethod occ-obj-rereadprop-value ((prop symbol)
                                        value)
  "Read org string property PROP to occ representation."
  (occ-assert (not (consp value)))
  (if (occ-obj-intf-list-p prop)
      (let* ((values (and value (split-string value))))
        (mapcar #'(lambda (v)
                    ;; from Org world to Occ world
                    (occ-obj-intf-from-org prop
                                           v))
                (mapcar #'org-entry-restore-space
                        values)))
    (occ-obj-intf-from-org prop
                           value)))

(cl-defmethod occ-obj-reread-props ((obj occ-tsk))
  "Read all org string properties for task TSK to occ representation."
  (let ((props-by-is-list   (occ-cl-method-param-case '(occ-obj-impl-list-p (`((eql ,val)) val))))
        (props-by-converter (occ-cl-method-param-case '(occ-obj-impl-from-org (`((eql ,val) t) val)))))
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


(cl-defmethod occ-obj-valid-p ((operation symbol)
                               (prop      symbol))
  (ignore prop)
  (memq operation
        '(add remove get put member)))


(cl-defmethod occ-obj-to-org-internal ((property symbol)
                                       build-list-p
                                       value)
  (if (occ-obj-intf-list-p prop)
      (if build-list-p
          (mapcar #'(lambda (v)
                      (occ-obj-intf-to-org prop v))
                  value)
        (occ-obj-intf-to-org prop value))
    (if build-list-p
        (let ((operation build-list-p))
          (occ-error "Property `%s' is not type of LIST, %s operation not applied to it." prop (upcase (symbol-name operation))))
      (occ-obj-intf-to-org prop value))))

(cl-defmethod occ-obj-to-org ((property symbol)
                              (operation symbol)
                              value)
  (occ-obj-to-org-internal property
                           nil
                           value))

(cl-defmethod occ-obj-to-org ((property symbol)
                              (operation null)
                              value)
  (occ-obj-to-org-internal property
                           nil
                           value))

(cl-defmethod occ-obj-to-org ((property symbol)
                              (operation (eql put))
                              value)
  (occ-obj-to-org-internal property
                           operation
                           value))

(cl-defmethod occ-obj-to-org ((property symbol)
                              (operation (eql t))
                              value)
  (occ-obj-to-org-internal property
                           operation
                           value))

(cl-defmethod occ-obj-to-org ((property symbol)
                              (operation (eql list))
                              value)
  (occ-obj-to-org-internal property
                           operation
                           value))

(cl-defmethod occ-obj-to-org ((property symbol)
                              (operation (eql delete))
                              value)
  (occ-obj-to-org-internal property
                           operatoin
                           value))


(cl-defmethod occ-obj-from-org-internal ((property symbol)
                                         build-list-p
                                         value)
  (if (occ-obj-intf-list-p prop)
      (if build-list-p
          (if (consp value)             ;BUG: TODO: cut value into list -- check if it is done in org-entry-get-multivalued-property
              (mapcar #'(lambda (v)
                          (occ-obj-intf-from-org prop v))
                      value)
            (occ-error "VALUE `%s' for PROPERTY `%s' is not list"
                       value
                       property))
        (occ-obj-intf-from-org prop value))
    (if build-list-p
        (let ((operation build-list-p))
          (occ-error "Property `%s' is not type of LIST, %s operation not applied to it." prop (upcase (symbol-name operation))))
      (occ-obj-intf-from-org prop value))))

(cl-defmethod occ-obj-from-org ((property symbol)
                                (operation symbol)
                                value)
  (occ-obj-from-org-internal property
                             nil
                             value))

(cl-defmethod occ-obj-from-org ((property symbol)
                                (operation null)
                                value)
  (occ-obj-from-org-internal property
                             nil
                             value))

(cl-defmethod occ-obj-from-org ((property symbol)
                                (operation (eql put))
                                value)
  (occ-obj-from-org-internal property
                             operation
                             value))

(cl-defmethod occ-obj-from-org ((property symbol)
                                (operation (eql t))
                                value)
  (occ-obj-from-org-internal property
                             operation
                             value))

(cl-defmethod occ-obj-from-org ((property symbol)
                                (operation (eql list))
                                value)
  (occ-obj-from-org-internal property
                             operation
                             value))

(cl-defmethod occ-obj-from-org ((property symbol)
                                (operation (eql delete))
                                value)
  (occ-obj-from-org-internal property
                             operatoin
                             value))
;; TODO: Implement Plist with title here (??)


;;; occ-prop.el ends here
