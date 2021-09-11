;;; occ-prop-edit.el --- property edit code          -*- lexical-binding: t; -*-

;; Copyright (C) 2021  sharad

;; Author: sharad <spratap@merunetworks.com>
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

(provide 'occ-prop-edit)


(require 'occ-prop-base)
(require 'occ-property-editor)


;; defined in occ-prop-intf.el
(cl-defmethod occ-operation ((obj  occ-obj-tsk)
                             (operation symbol)
                             (prop symbol)
                             values)
  "Accept occ compatible VALUES"
  (let ((mrk (occ-obj-marker obj)))
    (let ((retval (occ-org-call-operation-at-point mrk ;work in org file
                                                   prop
                                                   operation
                                                   ;; going to org world
                                                   (occ-prop-to-org prop
                                                                    values))))
      (occ-debug :debug "occ-operation: (occ-org-call-operation-at-point mrk) returnd %s" retval)
      (when retval                      ;; BUG: TODO: why calling again
        (if (cl-next-method-p)
            (cl-call-next-method)
          (occ-error "No occ-operation defined for prop %s operation %s" prop operation))))))


(cl-defmethod occ-operation ((obj       occ-obj-tsk)
                             (operation (eql get))
                             (prop symbol)
                             values)
  (let ((tsk (occ-obj-tsk obj)))
    (occ-message "(occ-operation occ-obj-tsk): operation %s prop %s" operation prop)
    (if (occ-list-p prop)
        (occ-get-property tsk
                          prop)
      (list (occ-get-property tsk
                              prop)))))

(cl-defmethod occ-operation ((obj       occ-obj-tsk)
                             (operation (eql add))
                             (prop symbol)
                             values)
  (let ((tsk    (occ-obj-tsk obj)))
    (occ-message "(occ-operation occ-obj-tsk): operation %s prop %s" operation prop)
    (if (occ-list-p prop)
        (occ-set-property tsk prop
                          (nconc (occ-get-property tsk prop)
                                 (list (car values))))
      (occ-set-property tsk prop
                        (car values)))))

(cl-defmethod occ-operation ((obj       occ-obj-tsk)
                             (operation (eql put))
                             (prop symbol)
                             values)
  (let ((tsk    (occ-obj-tsk obj)))
    (occ-message "(occ-operation occ-obj-tsk): operation %s prop %s" operation prop)
    (if (occ-list-p prop)
        (occ-set-property tsk prop
                          values)
      (occ-set-property tsk prop
                        (car values)))))

(cl-defmethod occ-operation ((obj       occ-obj-tsk)
                             (operation (eql remove))
                             (prop symbol)
                             values)
  (let ((tsk    (occ-obj-tsk obj)))
    (occ-message "(occ-operation occ-obj-tsk): operation %s prop %s" operation prop)
    (if (occ-list-p prop)
        (occ-set-property tsk prop
                          (remove (car values)
                                  (occ-get-property tsk prop)))
      (occ-error "Implement it."))))

(cl-defmethod occ-operation ((obj       occ-obj-tsk)
                             (operation (eql member))
                             (prop symbol)
                             values)
  (let ((tsk (occ-obj-tsk obj)))
    (occ-message "(occ-operation occ-obj-tsk): operation %s prop %s" operation prop)
    (occ-has-p tsk prop
               (car values))))


(cl-defgeneric occ-call-operation (obj
                                   prop
                                   operation
                                   values)
  "Accept occ compatible VALUES")

(cl-defmethod occ-call-operation ((obj  occ-obj-tsk)
                                  (operation symbol)
                                  (prop symbol)
                                  values)
  "Accept occ compatible VALUES"
  (occ-message "(occ-call-operation occ-obj-tsk): operation %s prop %s" operation prop)
  (occ-operation obj
                 operation
                 prop
                 values))


(cl-defgeneric occ-select-operation (obj prop)
  "occ-select-operation")

;; TODO: Add log not on property editing.
(cl-defmethod occ-select-operation ((obj  occ-obj-tsk)
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

(cl-defmethod occ-editprop ((obj  occ-obj-tsk)
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
    (occ-message "(occ-editprop occ-obj-tsk): operation %s prop %s" operation prop)
    (occ-call-operation obj
                        operation
                        prop
                        (if (consp prop-value)
                            prop-value
                          (list prop-value)))))


(cl-defgeneric occ-props-edit (obj)
  "Edit all property for forced clock-in.")

(cl-defmethod occ-props-edit ((obj occ-obj-tsk))
  "Edit all property for forced clock-in."
  (occ-properties-editor obj))

;;; occ-prop-edit.el ends here
