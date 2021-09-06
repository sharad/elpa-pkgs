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

(provide 'occ-prop)


(require 'dash)
;; (require 'subr)
(require 'org-misc-utils-lotus)


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


(require 'ert)
(require 'ert-x)
(require 'el-mock)


;; TODO: multi-value property https://orgmode.org/manual/Using-the-property-API.html


(ert-deftest ert-occ-test-match-prop-method-args ()
  "Test"
  :expected-result :passed
  :tags '(occ)
  (should (equal (cl-method-sigs-matched-arg
                  '(occ-readprop-from-user     (`(occ-obj-ctx-tsk (eql ,val)) val))
                  '(occ-get-property-value-from-ctx (`(occ-ctx (eql ,val)) val))
                  (occ-make-ctx-at-point))
                 '(timebeing)))
  ;; do this test in buffer of a temporary file.
  (should (equal (cl-method-sigs-matched-arg
                  '(occ-readprop-from-user     (`(occ-obj-ctx-tsk (eql ,val)) val))
                  '(occ-get-property-value-from-ctx (`(occ-ctx (eql ,val)) val))
                  (occ-make-ctx-at-point))
                 '(timebeing root currfile))))


(cl-defgeneric occ-properties-to-edit (obj)
  "return PROPERTIES list that can be edited.")
(cl-defgeneric occ-properties-to-inherit (obj)
  "return PROPERTIES list that can be inherited.")
(cl-defgeneric occ-properties-to-calculate-rank (obj)
  "return PROPERTIES list that can be used in calculating rank.")
(cl-defmethod occ-properties-to-checkout (obj)
  "return PROPERTIES list that can be checked-out.")


(cl-defmethod occ-properties-to-edit ((class symbol))
  "return PROPERTIES list that can be edited."
  (cl-method-param-values 'occ-readprop-from-user
                          (list '\` `(,class (eql ,'(\, val))))
                          'val))

(cl-defmethod occ-properties-to-edit ((obj occ-tsk))
  "return PROPERTIES list that can be edited."
  (cl-collect-on-classes #'occ-properties-to-edit
                         obj))

;; TODO: improve
(cl-defmethod occ-properties-to-edit ((obj occ-obj-ctx-tsk))
  "return PROPERTIES list that can be edited."
  (cl-method-sigs-matched-arg '(occ-readprop-from-user     (`(occ-obj-ctx-tsk (eql ,val)) val))
                              '(occ-get-property-value-from-ctx (`(occ-ctx (eql ,val)) val))
                              (occ-obj-ctx obj)))


(cl-defmethod occ-properties-to-inherit ((class symbol))
  "return PROPERTIES list that can be inherited."
  (cl-method-param-values 'occ-readprop-from-user
                          (list '\` `(,class (eql ,'(\, val))))
                          'val))

(cl-defmethod occ-properties-to-inherit ((obj occ-obj-tsk))
  "return PROPERTIES list that can be inherited."
  (cl-collect-on-classes #'occ-properties-to-inherit
                         obj))


(defun occ-readprop-props () ;;TODO: check about them
  "return PROPERTIES list that can be inherited."
  (occ-properties-to-inherit 'occ-obj-ctx-tsk))


(cl-defmethod occ-properties-to-calculate-rank ((class symbol))
  "return PROPERTIES list that can be used in calculating rank."
  (cl-method-param-values 'occ-rankprop
                          (list '\` `(,class (eql ,'(\, val))))
                          'val))

(cl-defmethod occ-properties-to-calculate-rank ((obj occ-obj-tsk))
  "return PROPERTIES list that can be used in calculating rank."
  (cl-collect-on-classes #'occ-properties-to-calculate-rank ;; occ-properties-to-calcuate-rank
                         obj))


(cl-defmethod occ-properties-to-checkout ((class symbol))
  "return PROPERTIES list that can be checked-out."
  (cl-method-param-values 'occ-checkoutprop
                          (list '\` `(,class (eql ,'(\, val))))
                          'val))

(cl-defmethod occ-properties-to-checkout ((obj occ-obj-tsk))
  "return PROPERTIES list that can be checked-out."
  (cl-collect-on-classes #'occ-properties-to-checkout
                         obj))


(defun occ-internal-remove-template-symbol (prop-list)
  (remove-if #'(lambda (prop)
                 (string-match "^_.+_$" (symbol-name prop)))
             prop-list))


(cl-defmethod occ-properties-to-edit :around (obj)
  "return PROPERTIES list that can be edited."
  (if (cl-next-method-p)
      (occ-internal-remove-template-symbol (cl-call-next-method))
    (occ-error "No
(cl-defmethod occ-properties-to-edit (obj)
  ...)

method provided.")))
(cl-defmethod occ-properties-to-inherit :around (obj)
  "return PROPERTIES list that can be inherited."
  (if (cl-next-method-p)
      (occ-internal-remove-template-symbol (cl-call-next-method))
    (occ-error "No
(cl-defmethod occ-properties-to-inherit (obj)
   ...)

method provided.")))
(cl-defmethod occ-properties-to-calculate-rank :around (obj)
  "return PROPERTIES list that can be used in calculating rank."
  (if (cl-next-method-p)
      (occ-internal-remove-template-symbol (cl-call-next-method))
    (occ-error "No
(cl-defmethod occ-properties-to-calculate-rank (obj)
  ...)

method provided.")))
(cl-defmethod occ-properties-to-checkout :around (obj)
  "return PROPERTIES list that can be checked-out."
  (if (cl-next-method-p)
      (occ-internal-remove-template-symbol (cl-call-next-method))
    (occ-error "No
(cl-defmethod occ-properties-to-checkout (obj)
  ...)

method provided.")))


;; NOTE: These two around methods not belongs to occ-prop-intf.el
;;       they belongs here only.
(cl-defmethod occ-readprop-from-user :around ((obj occ-obj-tsk)
                                              (prop symbol))
  "Read value of element of list for property PROP from user for
OCC-TSK OBJ."
  (if (cl-next-method-p)
      ;; (occ-prop-from-org prop
      ;;                    (cl-call-next-method))
      (cl-call-next-method)
    (occ-error "No
(cl-defmethod occ-readprop-from-user ((obj occ-obj-tsk) (prop (eql %s)))
  ...)

method provided."
               prop)))

(cl-defmethod occ-readprop-from-user :around ((obj occ-obj-tsk)
                                              (prop symbol))
  "Read value of element of list for property PROP from user for
OCC-TSK OBJ."
  (if (cl-next-method-p)
      ;; (occ-prop-from-org prop
      ;;                    (cl-call-next-method))
      (cl-call-next-method)
    (occ-error "No
(cl-defmethod occ-readprop-from-user ((obj occ-obj-tsk) (prop (eql %s)))
   ...)

method provided."
               prop)))


(cl-defmethod occ-rereadprop-value ((prop symbol)
                                    value)
  "Read org string property PROP to occ representation."
  (cl-assert (not (consp value)))
  (if (occ-list-p prop)
      (let* ((values (and value (split-string value))))
        (mapcar #'(lambda (v)
                    ;; from Org world to Occ world
                    (occ-prop-from-org prop
                                       v))
                (mapcar #'org-entry-restore-space
                        values)))
    (occ-prop-from-org prop
                       value)))

(cl-defmethod occ-reread-props ((obj occ-tsk))
  "Read all org string properties for task TSK to occ representation."
  (let ((props-by-is-list (cl-method-param-case
                           '(occ-list-p (`((eql ,val)) val))))
        (props-by-converter (cl-method-param-case
                             '(occ-prop-from-org (`((eql ,val) t) val)))))
    (let ((props (-union props-by-is-list
                         props-by-converter))) ;dash
      (dolist (p props)
        (occ-set-property obj p
                          (occ-rereadprop-value p
                                                (occ-get-property obj
                                                                  p)))))))

(cl-defmethod occ-reread-props :around (obj)
  "return PROPERTIES list that can be checked-out."
  (if (cl-next-method-p)
      (occ-internal-remove-template-symbol (cl-call-next-method))
    (occ-error "No
(cl-defmethod occ-reread-props (obj)
  ...)

method provided.")))


(cl-defmethod occ-require-p ((obj occ-obj-tsk)
                             (operation (eql get))
                             (prop      symbol)
                             values)
  nil)

(cl-defmethod occ-require-p ((obj occ-obj-tsk)
                             (operation (eql add))
                             (prop      symbol)
                             values)
  (not (occ-has-p obj prop
                  values)))

(cl-defmethod occ-require-p ((obj occ-obj-tsk)
                             (operation (eql put))
                             (prop      symbol)
                             values)
  nil)

(cl-defmethod occ-require-p ((obj occ-obj-tsk)
                             (operation (eql remove))
                             (prop      symbol)
                             values)
  (occ-has-p obj prop
             values))

(cl-defmethod occ-require-p ((obj occ-obj-tsk)
                             (operation (eql member))
                             (prop      symbol)
                             values)
  nil)


(cl-defmethod occ-valid-p ((prop symbol)
                           operation)
  (memq operation
        '(add remove get put member)))


(cl-defmethod occ-operations-for-prop ((class symbol)
                                       (prop symbol))
  ;; check about (occ-list-p prop) also
  (let ((ops (append (cl-method-param-values 'occ-operation
                                             (list '\` `(,class (eql ,'(\, val)) symbol t))
                                             'val)
                     (cl-method-param-values 'occ-operation
                                             (list '\` `(,class (eql ,'(\, val)) (eql ,prop) t))
                                             'val))))
    (delete-dups ops)))

(cl-defmethod occ-operations-for-prop ((obj  occ-obj-tsk)
                                       (prop symbol))
  ;; check about (occ-list-p prop) also
  (let ((ops (cl-collect-on-classes #'(lambda (class)
                                        (occ-operations-for-prop class
                                                                 prop))
                                    obj)))
    (delete-dups ops)))

(occ-testing
 (occ-operations-for-prop 'occ-obj-tsk 'root))


;; defined in occ-prop-intf.el
(cl-defmethod occ-operation ((obj occ-obj-tsk)
                             (prop symbol)
                             operation
                             values)
  "Accept occ compatible VALUES"
  (let ((mrk (occ-obj-marker obj)))
    (let ((retval (occ-org-call-operation-at-point mrk ;work in org file
                                                   prop
                                                   operation
                                                   ;; going to org world
                                                   (occ-prop-to-org prop
                                                                    values))))
      (occ-debug :debug "occ-editprop: (occ-org-call-operation-at-point mrk) returnd %s" retval)
      (when retval                      ;; BUG: TODO: why calling again
        ;; (occ-operation obj              ;now correct or reflect in occ objects.
        ;;                prop
        ;;                operation
        ;;                values)
        (when (cl-next-method-p)
          (cl-call-next-method)
          (occ-error "No occ-operation defined for prop %s operation %s" prop operation))))))


(cl-defmethod occ-operation ((obj occ-obj-tsk)
                             (operation (eql get))
                             (prop symbol)
                             values)
  (let ((tsk (occ-obj-tsk obj)))
    (if (occ-list-p prop)
        (occ-get-property tsk
                          prop)
      (list (occ-get-property tsk
                              prop)))))

(cl-defmethod occ-operation ((obj occ-obj-tsk)
                             (operation (eql add))
                             (prop symbol)
                             values)
  (let ((tsk    (occ-obj-tsk obj)))
    (if (occ-list-p prop)
        (occ-set-property tsk prop
                          (nconc (occ-get-property tsk prop)
                                 (list (car values))))
      (occ-set-property tsk prop
                        (car values)))))

(cl-defmethod occ-operation ((obj occ-obj-tsk)
                             (operation (eql put))
                             (prop symbol)
                             values)
  (let ((tsk    (occ-obj-tsk obj)))
    (if (occ-list-p prop)
        (occ-set-property tsk prop
                          values)
      (occ-set-property tsk prop
                        (car values)))))

(cl-defmethod occ-operation ((obj occ-obj-tsk)
                             (operation (eql remove))
                             (prop symbol)
                             values)
  (let ((tsk    (occ-obj-tsk obj)))
    (if (occ-list-p prop)
        (occ-set-property tsk prop
                          (remove (car values)
                                  (occ-get-property tsk prop)))
      (occ-error "Implement it."))))

(cl-defmethod occ-operation ((obj occ-obj-tsk)
                             (operation (eql member))
                             (prop symbol)
                             values)
  (let ((tsk (occ-obj-tsk obj)))
    (occ-has-p tsk prop
               (car values))))


(cl-defgeneric occ-call-operation (obj
                                   prop
                                   operation
                                   values)
  "Accept occ compatible VALUES")

(cl-defmethod occ-call-operation ((obj occ-obj-tsk)
                                  (prop symbol)
                                  operation
                                  values)
  "Accept occ compatible VALUES"
  (occ-operation obj
                 prop
                 operation
                 values))


(cl-defgeneric occ-select-operation (obj prop)
  "occ-select-operation")

;; TODO: Add log not on property editing.
(cl-defmethod occ-select-operation ((obj occ-obj-tsk)
                                    (prop symbol))
  (cl-assert prop)
  (if (occ-list-p prop)
      ;; TODO: where are generated actions?? (occ-operations-for-prop 'occ-obj-tsk 'root)
      (let* ((operations (occ-operations-for-prop obj prop))
             ;; (actions '(("add" . add)
             ;;            ("del" . remove)
             ;;            ("put" . put)))
             (actions    (mapcar #'(lambda (op)
                                     (cons (symbol-name op) op))
                                 operations)))
        (cl-assert actions)
        (let ((action  (completing-read (format "%s action: " prop) actions)))
          (cl-assert action)
          (cdr (assoc action
                      actions))))
    'put))


(cl-defgeneric occ-editprop (obj
                             prop
                             &optional
                             operation
                             value)
  "Accept occ compatible VALUES")

(cl-defmethod occ-editprop ((obj occ-obj-tsk)
                            (prop symbol)
                            &optional
                            operation
                            value)
  ;; TODO: change this to use OCC VALUE like with corresponding changes to occ-readprop-from-user
  "Accept occ compatible VALUES"
  (occ-debug :debug
             "occ-editprop: prop: %s, value: %s" prop value)
  (cl-assert prop)
  (let ((operation  (or operation
                        (occ-select-operation obj prop)))
        (prop-value (or value
                        (occ-readprop-from-user obj
                                                prop))))
    (cl-assert operation)
    (occ-call-operation obj
                        prop
                        operation
                        (if (consp prop-value)
                            prop-value
                          (list prop-value)))))


;; TODO: also accommodate increase decrease etc.
(cl-defmethod occ-gen-edit ((obj occ-obj-tsk)
                            (prop symbol)
                            (operation symbol)
                            value
                            &key param-only)
  "Used by occ-gen-prompt-edit"
  (if param-only
      (list prop
            operation
            value)
    #'(lambda (obj)
        (occ-editprop obj
                      prop
                      operation
                      value))))

(cl-defmethod occ-gen-prompt ((obj occ-obj-tsk)
                              (prop symbol)
                              (operation symbol)
                              value)
  "Used by occ-gen-prompt-edit"
  ;; TODO: Improve it.
  (let ((list-p (occ-list-p prop)))
    (format "%s %s %s property %s"
            (symbol-name operation)
            (occ-format-prop obj prop value)
            (if list-p "in" "from")
            prop)))


(cl-defgeneric occ-gen-prompt-edit (obj
                                    prop
                                    operation
                                    value
                                    &key param-only)
  "occ-gen-prompt-edit")

(cl-defmethod occ-gen-prompt-edit ((obj occ-obj-tsk)
                                   (prop symbol)
                                   (operation symbol)
                                   value
                                   &key param-only)
  (let ((prompt    (occ-gen-prompt obj
                                   prop
                                   operation
                                   value))
        (operation (occ-gen-edit obj
                                 prop
                                 operation
                                 value
                                 :param-only param-only)))
    (occ-make-callable-normal :edit-1-generated
                              prompt
                              operation)))


(cl-defmethod occ-gen-edit-if-required ((obj occ-obj-tsk)
                                        (prop symbol)
                                        (operation symbol)
                                        value
                                        &key param-only)
  (when (occ-require-p obj
                       operation
                       prop
                       value)
    (occ-gen-prompt-edit obj
                         prop
                         operation
                         value
                         :param-only param-only)))


(cl-defmethod occ-gen-edits-if-required ((obj       occ-obj-tsk)
                                         (prop      symbol)
                                         (operation null)
                                         &key param-only)
  (let* ((ops      (occ-operations-for-prop obj prop))
         (edit-ops (mapcar #'(lambda (operation)
                               (let ((value (occ-prop-default-value obj
                                                                    prop
                                                                    operation)))
                                 (when value
                                   (occ-gen-edit-if-required obj
                                                             prop
                                                             operation
                                                             value
                                                             :param-only param-only))))
                           ops)))
    (remove nil edit-ops)))

(cl-defmethod occ-gen-edits-if-required ((obj occ-obj-tsk)
                                         (prop null)
                                         (operation symbol)
                                         &key param-only)
  (let* ((ops      (occ-properties-to-edit obj))
         (edit-ops (mapcar #'(lambda (prop)
                               (occ-gen-edits-if-required obj
                                                          prop
                                                          operation
                                                          :param-only param-only))
                           ops)))
    (remove nil edit-ops)))

(cl-defmethod occ-gen-edits-if-required ((obj occ-obj-tsk)
                                         (prop null)
                                         (operation null)
                                         &key param-only)
  (let* ((ops      (occ-properties-to-edit obj))
         (edit-ops (mapcar #'(lambda (prop)
                               (occ-gen-edits-if-required obj
                                                          prop
                                                          operation
                                                          :param-only param-only))
                           ops)))
    (apply #'append
           edit-ops)))


(cl-defgeneric occ-checkout (obj)
  "Checkout property for forced clock-in.")

(cl-defmethod occ-checkout ((obj occ-obj-tsk))
  "Checkout property for forced clock-in."
  (dolist (prop (occ-properties-to-checkout obj))
    (occ-message "occ-checkout: checkout prop %s" prop)
    (occ-checkout-prop obj prop)))


(cl-defmethod occ-gen-checkout-prompt ((obj occ-obj-tsk)
                                       (prop      symbol)
                                       &key param-only)
  (let ((list-p (occ-list-p prop)))
    (format "Checkout property %s"
            prop)))

(cl-defmethod occ-gen-checkout ((obj occ-obj-tsk)
                                (prop      symbol)
                                &key param-only)
  (if param-only
      (list prop)
    #'(lambda (obj)
        (occ-checkoutprop obj prop))))


(cl-defmethod occ-gen-checkout-if-required ((obj occ-obj-tsk)
                                            (prop      symbol)
                                            &key param-only)
  (if (occ-required-p obj               ;TODO: ctx is require, and resolve function collision.
                      prop)
      (occ-gen-checkout obj
                        prop
                        :param-only param-only)))


(cl-defmethod occ-gen-checkouts ((obj null)
                                 &param-only param-only)
  nil)

(cl-defmethod occ-gen-checkouts ((obj occ-obj-ctx-tsk)
                                 &param-only param-only)
  (occ-gen-checkouts obj))

(cl-defmethod occ-gen-checkouts ((obj occ-obj-tsk))
  (remove nil
          (mapcar #'occ-gen-checkout-if-required
                  (occ-properties-to-checkout (occ-obj-tsk obj)))))


(cl-defmethod occ-gen-fast-edits ((obj null)
                                  &key param-only)
  nil)

(cl-defmethod occ-gen-fast-edits ((obj occ-obj-ctx-tsk)
                                  &key param-only)
  (occ-gen-edits-if-required obj nil nil
                             :param-only param-only))


(cl-defmethod occ-gen-edits ((obj null)
                             &param-only param-only)
  nil)

(cl-defmethod occ-gen-edits ((obj occ-obj-ctx-tsk)
                             &param-only param-only)
  (list (occ-make-callable-normal :edit
                                  "Edit"
                                  #'(lambda (obj)
                                      (occ-props-edit obj)))))



;; Correct it ???
(cl-defmethod occ-gen-misc ((obj null)
                            &param-only param-only)
  (list (occ-make-callable-normal :continue "Continue" t)
        (occ-make-callable-normal :checkout "Checkout" occ-checkout)))

(cl-defmethod occ-gen-misc ((obj occ-obj-ctx-tsk)
                            &param-only param-only)
  (list (occ-make-callable-normal :continue "Continue" t)
        (occ-make-callable-normal :checkout "Checkout" occ-checkout)))



;; TODO: Implement Plist with title here (??)


;;; occ-prop.el ends here
