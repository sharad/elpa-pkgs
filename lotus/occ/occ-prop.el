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

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Generic-Functions.html

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


(require 'ert)
(require 'ert-x)
(require 'el-mock)


;; TODO: multi-value property https://orgmode.org/manual/Using-the-property-API.html


;; (cl-defgeneric occ-match-prop-method-args (obj)
;;   "occ-match-prop-method-args")

;; (cl-defmethod occ-match-prop-method-args ((obj occ-tsk))
;;   (cl-method-param-case '(occ-readprop-elem-from-user (`(occ-tsk (eql ,val)) val))))

;; (cl-defmethod occ-match-prop-method-args ((obj occ-obj-ctx-tsk))
;;   (cl-method-sigs-matched-arg
;;    ;; '(occ-readprop-with (`(occ-tsk occ-ctx (eql ,val)) val))
;;    '(occ-readprop-elem-from-user (`(occ-obj-ctx-tsk (eql ,val)) val))
;;    '(occ-get-property  (`(occ-ctx (eql ,val)) val))
;;    (occ-obj-ctx obj)))

(ert-deftest ert-occ-test-match-prop-method-args ()
  "Test"
  :expected-result :passed
  :tags '(occ)
  (should
   (equal
    (cl-method-sigs-matched-arg
     '(occ-readprop-elem-from-user (`(occ-obj-ctx-tsk (eql ,val)) val))
     '(occ-get-property  (`(occ-ctx (eql ,val)) val))
     (occ-make-ctx-at-point))
    '(timebeing)))

  ;; do this test in buffer of a temporary file.
  (should
   (equal
    (cl-method-sigs-matched-arg
     '(occ-readprop-elem-from-user (`(occ-obj-ctx-tsk (eql ,val)) val))
     '(occ-get-property  (`(occ-ctx (eql ,val)) val))
     (occ-make-ctx-at-point))
    '(timebeing root currfile))))


(cl-defgeneric occ-properties-to-edit (obj)
  "occ-properties-to-edit")

(cl-defgeneric occ-properties-to-inherit (obj)
  "occ-properties-to-inherit")

(cl-defgeneric occ-properties-to-calculate-rank (obj)
  "occ-properties-to-calculate-rank")


(cl-defmethod occ-properties-to-edit ((class symbol))
  (cl-method-param-values 'occ-readprop-elem-from-user
                          (list '\` `(,class (eql ,'(\, val))))
                          'val))

(cl-defmethod occ-properties-to-edit ((obj occ-tsk))
  (cl-collect-on-classes #'occ-properties-to-edit
                         obj))

;; TODO: improve
(cl-defmethod occ-properties-to-edit ((obj occ-obj-ctx-tsk))
  (cl-method-sigs-matched-arg
   '(occ-readprop-elem-from-user (`(occ-obj-ctx-tsk (eql ,val)) val))
   '(occ-get-property            (`(occ-ctx (eql ,val)) val))
   (occ-obj-ctx obj)))


(cl-defmethod occ-properties-to-inherit ((class symbol))
  (cl-method-param-values 'occ-readprop-elem-from-user
                          (list '\` `(,class (eql ,'(\, val))))
                          'val))

(cl-defmethod occ-properties-to-inherit ((obj occ-obj-tsk))
  (cl-collect-on-classes #'occ-properties-to-inherit
                         obj))


(defun occ-readprop-props () ;;TODO: check about them
  (occ-properties-to-inherit 'occ-obj-ctx-tsk))


(cl-defmethod occ-properties-to-calcuate-rank ((class symbol))
  (cl-method-param-values 'occ-rankprop
                          (list '\` `(,class (eql ,'(\, val))))
                          'val))

(cl-defmethod occ-properties-to-calculate-rank ((obj occ-obj-tsk))
  (cl-collect-on-classes #'occ-properties-to-calcuate-rank
                         obj))


(cl-defmethod occ-properties-to-checkout ((class symbol))
  (cl-method-param-values 'occ-checkoutprop
                          (list '\` `(,class (eql ,'(\, val))))
                          'val))

(cl-defmethod occ-properties-to-checkout ((obj occ-obj-tsk))
  (cl-collect-on-classes #'occ-properties-to-checkout
                         obj))


(defun occ-org-entry-get (pom
                          prop)
  (lotus-org-with-safe-modification
    (org-entry-get pom
                   prop)))

(defun occ-org-entry-put (pom
                          prop
                          value)
  (lotus-org-with-safe-modification
    (org-entry-put pom
                   prop
                   value)))

(defun occ-org-entry-get-multivalued-property (pom
                                               prop)
  (lotus-org-with-safe-modification
    (org-entry-get-multivalued-property pom prop)))

(defun occ-org-entry-put-multivalued-property (pom
                                               prop
                                               values)
  (lotus-org-with-safe-modification
    (apply #'org-entry-put-multivalued-property
           pom prop values)))

(defun occ-org-entry-add-to-multivalued-property (pom
                                                  prop
                                                  value)
  (lotus-org-with-safe-modification
    (org-entry-add-to-multivalued-property pom
                                           prop
                                           value)
    t))

(defun occ-org-entry-remove-from-multivalued-property (pom
                                                       prop
                                                       value)
  (lotus-org-with-safe-modification
    (org-entry-remove-from-multivalued-property pom
                                                prop
                                                value)
    t))

(defun occ-org-entry-member-in-multivalued-property (pom
                                                     prop
                                                     values)
  (lotus-org-with-safe-modification
    (org-entry-member-in-multivalued-property pom
                                              prop
                                              values)))


(cl-defmethod occ-prop-to-org ((prop symbol)
                               values)
  "Method convert value VALUE of property PROP from occ to org string representation."
  ;; (error "Implement method occ-prop-to-org for prop %s" prop)
  (occ-debug :debug "occ-prop-to-org: no method for prop %s using default." prop)
  (mapcar #'(lambda (v)
              (occ-prop-elem-to-org prop v))
          values))
(cl-defmethod occ-prop-from-org ((prop symbol)
                                 values)
  "Method convert value VALUE of property PROP from org string to occ representation."
  ;; (error "Implement method occ-prop-from-org for prop %s" prop)
  (occ-debug :debug
             "occ-prop-from-org: no method for prop %s using default." prop)
  (mapcar #'(lambda (v)
              (occ-prop-elem-from-org prop v))
          values))


(cl-defmethod occ-readprop-elem-from-user :around ((obj occ-obj-tsk)
                                                   (prop symbol))
              "Read value of element of list for property PROP from user for OCC-TSK OBJ."
              (occ-prop-elem-from-org (call-next-method)))

(cl-defmethod occ-readprop-from-user :around ((obj occ-obj-tsk)
                                              (prop symbol))
              "Read value of element of list for property PROP from user for OCC-TSK OBJ."
              (occ-prop-from-org (call-next-method)))


(cl-defmethod occ-rereadprop-value ((prop symbol)
                                    value)
  "Read org string property PROP to occ representation."
  (cl-assert (not (consp value)))
  (if (occ-list-p prop)
      (let* ((values (and value (split-string value))))
        (mapcar #'(lambda (v) (occ-prop-elem-from-org prop v))
                (mapcar #'org-entry-restore-space values)))
    (occ-prop-elem-from-org prop value)))

(cl-defmethod occ-reread-props ((obj occ-tsk))
  "Read all org string properties for task TSK to occ representation."
  (let ((props-by-is-list (cl-method-param-case
                           '(occ-list-p (`((eql ,val)) val))))
        (props-by-converter (cl-method-param-case
                             '(occ-prop-elem-from-org (`((eql ,val) t) val)))))
    (let ((props (-union props-by-is-list props-by-converter))) ;dash
      (dolist (p props)
        (occ-set-property obj p
                          (occ-rereadprop-value p (occ-get-property obj p)))))))


(cl-defmethod occ-valid-p ((prop symbol)
                           operation)
  (memq operation
        '(add remove get put member)))


(cl-defgeneric occ-org-operation (pom
                                  operation
                                  prop
                                  values)
  "occ-org-operation")

(cl-defmethod occ-org-operation ((pom  marker)
                                 (operation (eql get))
                                 (prop symbol)
                                 values)
  (let ((prop-string (symbol-name prop)))
    (if (occ-list-p prop)
        (occ-org-entry-get-multivalued-property pom prop-string)
      (list (occ-org-entry-get pom prop-string)))))

(cl-defmethod occ-org-operation ((pom  marker)
                                 (operation (eql add))
                                 (prop symbol)
                                 values)
  (let ((prop-string (symbol-name prop)))
    (if (occ-list-p prop)
        (occ-org-entry-add-to-multivalued-property pom prop-string (car values))
      (occ-org-entry-put pom prop-string (car values)))))

(cl-defmethod occ-org-operation ((pom  marker)
                                 (operation (eql put))
                                 (prop symbol)
                                 values)
  (let ((prop-string (symbol-name prop)))
    (if (occ-list-p prop)
        (occ-org-entry-put-multivalued-property pom prop-string values)
      (occ-org-entry-put pom prop-string (car values)))))

(cl-defmethod occ-org-operation ((pom  marker)
                                 (operation (eql remove))
                                 (prop symbol)
                                 values)
  (let ((prop-string (symbol-name prop)))
    (if (occ-list-p prop)
        (occ-org-entry-remove-from-multivalued-property pom prop-string (car values))
      (error "Implement it."))))

(cl-defmethod occ-org-operation ((pom  marker)
                                 (operation (eql member))
                                 (prop symbol)
                                 values)
  (let ((prop-string (symbol-name prop)))
    (if (occ-list-p prop)
        (occ-org-entry-member-in-from-multivalued-property pom prop-string (car values))
      (string= (car values) (occ-org-entry-get pom prop-string)))))


(cl-defmethod occ-org-operation ((obj occ-obj-tsk)
                                 (operation symbol)
                                 (prop symbol)
                                 values)
  (occ-org-operation (occ-obj-marker obj)
                     operation
                     prop
                     values))


(cl-defgeneric occ-org-update-property (pom
                                        prop
                                        operation
                                        values)
  "occ-org-update-property")

(cl-defmethod occ-org-update-property ((pom  marker)
                                       (prop symbol)
                                       (operation symbol)
                                       values)
  "Accept org compatible VALUES"
  ;; (unless (occ-valid-p prop operation)
  ;;   (error "occ-org-update-property: operation %s is not allowed for prop %s" operation prop))
  (occ-org-operation pom
                     prop
                     operation
                     values))

(cl-defmethod occ-org-update-property-at-point ((mrk  marker)
                                                (prop symbol)
                                                operation
                                                values)
  "Accept org compatible VALUES"
  (unless (occ-valid-p prop operation)
    (error "occ-org-update-property: operation %s is not allowed for prop %s" operation prop))
  (lotus-with-marker mrk
    (unless (org-get-property-block)
      ;; create property drawer
      ;; TODO: NOTE: only create property block if 100% sure value is going to be set.
      (occ-debug :debug "occ-org-update-property-at-point: property block not exist so creating it.")
      (let* ((range (org-get-property-block (point) 'force))
             (start (when (consp range) (1- (car range)))))
        (if (and range start)
            (when (numberp start)
              (goto-char start))
          (error "occ-org-update-property-at-point: not able to create property block to add property %s: %s"
                 prop values))))

    (if (org-get-property-block)
        (progn
          (occ-debug :debug
                     "occ-org-update-property-at-point: adding prop: %s value: %s using (org-set-property)."
                     prop values)
          (let ((retval (occ-org-update-property mrk
                                                 prop
                                                 operation
                                                 values)))
            (occ-debug :debug "occ-org-update-property: (occ-org-update-property mrk) returned %s" retval)
            retval))
        (error "occ-org-update-property-at-point: can not get property block to add property %s: %s"
               prop values))))


(cl-defmethod occ-readprop-org ((obj occ-obj-ctx-tsk)
                                (prop symbol))
  "Read property PROP of OBJ-CTX-TSK OBJ from its corresponding org file entry."
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (let* ((mrk    (or (occ-obj-marker tsk) (point)))
           (values (occ-org-update-property-at-point mrk
                                                     prop
                                                     'get)))
      (mapcar #'(lambda (v)
                  (occ-prop-elem-from-org prop v))
              values))))

(cl-defmethod occ-writeprop-org ((obj occ-obj-ctx-tsk)
                                 (prop symbol))
  "Write property PROP of OBJ-CTX-TSK OBJ from its corresponding org file entry."
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (let* ((values (occ-get-property tsk prop))
           (values (if (consp values) values (list values)))
           (values (mapcar #'(lambda (v)
                               (occ-prop-elem-to-org prop v))
                           values)))
      (occ-org-update-property-at-point (point)
                                        prop
                                        'put
                                        values))))


(cl-defgeneric occ-has-p (obj
                          prop
                          value)
  "occ-has-p")

(cl-defmethod occ-has-p ((obj occ-obj-tsk)
                         (prop symbol)
                         value)
  (let ((tsk (occ-obj-tsk obj)))
    (if (occ-list-p prop)
        (memq value (occ-get-property tsk prop))
      (equal value (occ-get-property tsk prop)))))


(cl-defmethod occ-operations-for-prop ((class symbol)
                                       (prop symbol))
  (delete-dups
   (append
    (cl-method-param-values 'occ-operation
                            (list '\` `(,class (eql ,'(\, val)) symbol t))
                            'val)
    (cl-method-param-values 'occ-operation
                            (list '\` `(,class (eql ,'(\, val)) (eql ,prop) t))
                            'val))))

(cl-defmethod occ-operations-for-prop ((obj  occ-obj-tsk)
                                       (prop symbol))
  (delete-dups
   (cl-collect-on-classes #'(lambda (class) (occ-operations-for-prop class prop))
                          obj)))


(cl-defmethod occ-operation ((obj occ-obj-tsk)
                             (operation (eql get))
                             (prop symbol)
                             values)
  (let ((tsk    (occ-obj-tsk obj)))
    (if (occ-list-p prop)
        (occ-get-property tsk prop)
      (list (occ-get-property tsk prop)))))

(cl-defmethod occ-operation ((obj occ-obj-tsk)
                             (operation (eql add))
                             (prop symbol)
                             values)
  (let ((tsk    (occ-obj-tsk obj)))
    (if (occ-list-p prop)
        (occ-set-property tsk prop
                          (nconc
                           (occ-get-property tsk prop)
                           (list (car values))))
      (occ-set-property tsk prop (car values)))))

(cl-defmethod occ-operation ((obj occ-obj-tsk)
                             (operation (eql put))
                             (prop symbol)
                             values)
  (let ((tsk    (occ-obj-tsk obj)))
    (if (occ-list-p prop)
        (occ-set-property tsk prop values)
      (occ-set-property tsk prop (car values)))))

(cl-defmethod occ-operation ((obj occ-obj-tsk)
                             (operation (eql remove))
                             (prop symbol)
                             values)
  (let ((tsk    (occ-obj-tsk obj)))
    (if (occ-list-p prop)
        (occ-set-property tsk prop (remove
                                    (car values)
                                    (occ-get-property tsk prop)))
      (error "Implement it."))))

(cl-defmethod occ-operation ((obj occ-obj-tsk)
                             (operation (eql member))
                             (prop symbol)
                             values)
  (let ((tsk    (occ-obj-tsk obj)))
    (occ-has-p tsk prop (car values))))


(cl-defmethod occ-require-p ((obj occ-obj-tsk)
                             (operation (eql get))
                             (prop      symbol)
                             values)
  nil)

(cl-defmethod occ-require-p ((obj occ-obj-tsk)
                             (operation (eql add))
                             (prop      symbol)
                             values)
  (not (occ-has-p obj prop values)))

(cl-defmethod occ-require-p ((obj occ-obj-tsk)
                             (operation (eql put))
                             (prop      symbol)
                             values)
  nil)

(cl-defmethod occ-require-p ((obj occ-obj-tsk)
                             (operation (eql remove))
                             (prop      symbol)
                             values)
  (occ-has-p obj prop values))

(cl-defmethod occ-require-p ((obj occ-obj-tsk)
                             (operation (eql member))
                             (prop      symbol)
                             values)
  nil)

(cl-defgeneric occ-update-property (obj
                                    prop
                                    operation
                                    values)
  "Accept occ compatible VALUES")

(cl-defmethod occ-update-property ((obj occ-obj-tsk)
                                   (prop symbol)
                                   operation
                                   values)
  "Accept occ compatible VALUES"
  (let ((mrk (occ-obj-marker obj)))
    (let ((retval
           (occ-org-update-property-at-point mrk
                                             prop
                                             operation
                                             (occ-prop-to-org prop values))))
      (occ-debug :debug "occ-editprop: (occ-org-update-property-at-point mrk) returnd %s" retval)
      (when retval
        (occ-operation obj
                       prop
                       operation
                       values)))))


(cl-defgeneric occ-select-operation (prop)
  "occ-select-operation")

;; TODO: Add log not on property editing.
(cl-defmethod occ-select-operation ((prop symbol))
  (if (occ-list-p prop)
      (let ((actions '(("add" . add)
                       ("del" . remove)
                       ("put" . put))))
        (cdr
         (assoc
          (completing-read (format "%s action: " prop) actions)
          actions)))
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
  ;; TODO: change this to use OCC VALUE like with corresponding changes to occ-readprop-elem-from-user
  "Accept occ compatible VALUES"
  (occ-debug :debug
             "occ-editprop: prop: %s, value: %s" prop value)
  (let ((operation  (or operation (occ-select-operation prop)))
        (prop-value (or value     (occ-readprop-elem-from-user obj prop))))
    (occ-update-property obj
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
  (if param-only
      (list prop operation value)
    #'(lambda (obj)
        (occ-editprop obj
                      prop
                      operation
                      value))))


(cl-defmethod occ-gen-prompt ((obj occ-obj-tsk)
                              (prop symbol)
                              (operation symbol)
                              value)
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
  (cons
   (occ-gen-prompt obj
                   prop
                   operation
                   value)
   (occ-gen-edit obj
                 prop
                 operation
                 value
                 :param-only param-only)))


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
  (remove nil
          (mapcar #'(lambda (operation)
                      (let ((value (occ-prop-default-value obj
                                                           prop
                                                           operation)))
                        (when value
                          (occ-gen-edit-if-required obj
                                                    prop
                                                    operation
                                                    value
                                                    :param-only param-only))))
                  (occ-operations-for-prop obj prop))))

(cl-defmethod occ-gen-edits-if-required ((obj occ-obj-tsk)
                                         (prop null)
                                         (operation symbol)
                                         &key param-only)
  (remove nil
          (mapcar #'(lambda (prop)
                      (occ-gen-edits-if-required obj
                                                 prop
                                                 operation
                                                :param-only param-only))
                  (occ-properties-to-edit obj))))

(cl-defmethod occ-gen-edits-if-required ((obj occ-obj-tsk)
                                         (prop null)
                                         (operation null)
                                         &key param-only)
  (apply #'append
         (mapcar #'(lambda (prop)
                     (occ-gen-edits-if-required obj
                                                prop
                                                operation
                                                :param-only param-only))
                 (occ-properties-to-edit obj))))


(cl-defgeneric occ-checkout (obj)
  "Checkout property in case of force clock-in.")

(cl-defmethod occ-checkout ((obj occ-obj-tsk))
  "Checkout property in case of force clock-in."
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


(cl-defmethod occ-gen-checkouts ((obj occ-obj-tsk))
  (remove nil
          (mapcar #'occ-gen-checkout-if-required
                  (occ-properties-to-checkout obj))))


(cl-defmethod occ-gen-helm-fast-edits ((obj null)
                                       &key param-only)
  nil)

(cl-defmethod occ-gen-helm-fast-edits ((obj occ-obj-ctx-tsk)
                                       &key param-only)
  (occ-gen-edits-if-required obj nil nil
                             :param-only param-only))


(cl-defmethod occ-gen-helm-edits ((obj null) &param-only param-only)
  nil)

(cl-defmethod occ-gen-helm-edits ((obj occ-obj-ctx-tsk) &param-only param-only)
  (list
   (cons "Edit" #'(lambda () (occ-props-edit obj)))))


(cl-defmethod occ-gen-helm-misc ((obj null) &param-only param-only)
  '(("Continue" . t)
    ("Checkout" . checkout)))

(cl-defmethod occ-gen-helm-misc ((obj occ-obj-ctx-tsk) &param-only param-only)
  '(("Continue" . t)
    ("Checkout" . checkout)))


(cl-defmethod occ-gen-helm-checkouts ((obj null) &param-only param-only)
  nil)

(cl-defmethod occ-gen-helm-checkouts ((obj occ-obj-ctx-tsk) &param-only param-only)
  (occ-gen-checkouts obj))


;; TODO: Implement Plist with title here


;;; occ-prop.el ends here
