;;; occ-prop-gen-edit-actions.el --- generate edit helm actions for property  -*- lexical-binding: t; -*-

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

(provide 'occ-prop-gen-edit-actions)


(require 'occ-prop-base)
(require 'occ-prop-op-edit)


;; TODO: also accommodate increase decrease etc.
(cl-defmethod occ-gen-edit-prompt ((obj       occ-obj-tsk)
                                   (prop      symbol)
                                   (operation symbol)
                                   value
                                   &key param-only)
  "Used by occ-gen-edit"
  ;; TODO: Improve it.
  (let ((list-p (occ-list-p prop)))
    (format "%s %s %s property %s to %s"
            (symbol-name operation)
            (occ-format-prop obj prop value)
            (if list-p "in" "from")
            prop
            (occ-Format obj))))

(cl-defmethod occ-gen-edit-fun ((obj       occ-obj-tsk)
                                (prop      symbol)
                                (operation symbol)
                                value
                                &key param-only)
  "Generate helm function, purpose PARAM-ONLY for the case where only argument required for some other further processing"
  (if param-only
      (list prop
            operation
            value)
    #'(lambda (candidate)
        (occ-op-prop-edit obj
                          prop
                          operation
                          value))))


(cl-defgeneric occ-gen-edit (obj
                             prop
                             operation
                             value
                             &key param-only)
  "occ-gen-edit")

(cl-defmethod occ-gen-edit ((obj       occ-obj-tsk)
                            (prop      symbol)
                            (operation symbol)
                            value
                            &key param-only)
  (occ-message "occ-gen-edit: checking prop %s operation %s" prop operation)
  (let ((prompt  (occ-gen-edit-prompt obj
                                      prop
                                      operation
                                      value
                                      :param-only param-only))
        (fun     (occ-gen-edit-fun obj
                                   prop
                                   operation
                                   value
                                   :param-only param-only))
        (keyword (sym2key (gensym))))
    (occ-make-callable-normal keyword
                              prompt
                              fun)))


(cl-defmethod occ-gen-edit-if-required ((obj       occ-obj-tsk)
                                        (prop      symbol)
                                        (operation symbol)
                                        value
                                        &key param-only)
  (when (occ-require-p obj
                       operation
                       prop
                       value)
    (occ-gen-edit obj
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
    (remove nil
            edit-ops)))

(cl-defmethod occ-gen-edits-if-required ((obj       occ-obj-tsk)
                                         (prop      null)
                                         (operation symbol)
                                         &key param-only)
  ;; NOTE: occ-properties-to-edit will handle (obj occ-obj-ctx-tsk)
  (let* ((props    (occ-properties-to-edit obj))
         ;; will be call (OCC-GEN-EDITS-IF-REQUIRED OBJ PROP OPERATION :PARAM_ONLY PARAM_ONLY)
         (edit-ops (mapcar #'(lambda (prop)
                               (occ-gen-edits-if-required obj
                                                          prop
                                                          operation
                                                          :param-only param-only))
                           props)))
    (remove nil
            edit-ops)))

(cl-defmethod occ-gen-edits-if-required ((obj       occ-obj-tsk)
                                         (prop      null)
                                         (operation null)
                                         &key param-only)
  (let* ((props    (occ-properties-to-edit obj))
         ;; NOTE:
         ;; will be calling            (OCC-GEN-EDITS-IF-REQUIRED OBJ PROP NIL :PARAM_ONLY PARAM_ONLY)
         ;; which in turn will be call (OCC-GEN-EDITS-IF-REQUIRED OBJ PROP OPERATION :PARAM_ONLY PARAM_ONLY)
         (edit-ops (mapcar #'(lambda (prop)
                               (occ-gen-edits-if-required obj
                                                          prop
                                                          operation ;nil
                                                          :param-only param-only))
                           props)))
    (apply #'append
           edit-ops)))


(cl-defmethod occ-gen-each-prop-edits ((obj null)
                                       &key param-only)
  nil)

(cl-defmethod occ-gen-each-prop-edits ((obj occ-obj-tsk)
                                       &key param-only)
  ;; NOTE:
  ;; will call (OCC-GEN-EDITS-IF-REQUIRED ((OBJ OCC-OBJ-TSK) (PROP NULL) (OPERATION NULL) &KEY PARAM-ONLY)
  ;; function as number of arguments are different.
  (occ-gen-edits-if-required obj nil nil
                             :param-only param-only))

(cl-defmethod occ-gen-each-prop-edits ((obj occ-obj-ctx-tsk)
                                       &key param-only)
  ;; NOTE:
  ;; will call (OCC-GEN-EDITS-IF-REQUIRED ((OBJ OCC-OBJ-TSK) (PROP NULL) (OPERATION NULL) &KEY PARAM-ONLY)
  ;; function as number of arguments are different.
  (occ-gen-edits-if-required obj nil nil
                             :param-only param-only))

(cl-defmethod occ-gen-each-prop-edits ((obj occ-obj-ctx)
                                       &key param-only)
  nil)


(defun* occ-gen-each-prop-fast-edits (obj &key param-only)
  (occ-gen-each-prop-edits obj :param-only param-only))


(cl-defmethod occ-gen-simple-edits ((obj null)
                                    &key param-only)
  nil)

(cl-defmethod occ-gen-simple-edits ((obj occ-obj-tsk)
                                    &key param-only)
  (list (occ-make-callable-normal :edit
                                  (format "Edit %s" (occ-Format obj))
                                  #'(lambda (obj)
                                      (occ-op-props-edit obj)))))

(cl-defmethod occ-gen-simple-edits ((obj occ-obj-ctx-tsk)
                                    &key param-only)
  (list (occ-make-callable-normal :edit
                                  (format "Edit %s" (occ-Format obj))
                                  #'(lambda (obj)
                                      (occ-op-props-edit obj)))))

(cl-defmethod occ-gen-simple-edits ((obj occ-obj-ctx)
                                    &key param-only)
  nil)

;;; occ-prop-gen-edit-actions.el ends here
