;;; occ-prop-gen-checkout-actions.el --- generate checkout helm actions for property  -*- lexical-binding: t; -*-

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

(provide 'occ-prop-gen-checkout-actions)


(require 'occ-prop-base)
(require 'occ-prop-op-checkout)


(cl-defmethod occ-gen-checkout-prompt ((obj  occ-obj-tsk)
                                       (prop symbol)
                                       &key param-only)
  "Used by occ-gen-checkout"
  (let ((list-p (occ-list-p prop)))
    (format "%s property %s of %s"
            "Checkout"
            prop
            (occ-Format obj))))

(cl-defmethod occ-gen-checkout-fun ((obj  occ-obj-tsk)
                                    (prop symbol)
                                    &key param-only)
  "Generate helm function, purpose PARAM-ONLY for the case where
only argument required for some other further processing"
  (if param-only
      (list prop)
    #'(lambda (obj)
        (occ-op-prop-checkout obj prop))))


(cl-defgeneric occ-gen-checkout (obj
                                 prop
                                 &key param-only)
  "occ-gen-checkout")

(cl-defmethod occ-gen-checkout ((obj       occ-obj-tsk)
                                (prop      symbol)
                                &key param-only)
  (occ-message "occ-gen-checkout: checking prop %s" prop)
  (let ((prompt  (occ-gen-checkout-prompt obj
                                          prop
                                          :param-only param-only))
        (fun     (occ-gen-checkout-fun obj
                                       prop
                                       :param-only param-only))
        (keyword (sym2key (gensym))))
    (occ-make-callable-normal keyword
                              prompt
                              fun)))


(cl-defmethod occ-gen-checkout-if-required ((obj  occ-obj-tsk)
                                            (prop symbol)
                                            &key param-only)
  (if (occ-get-property obj prop)
      (occ-gen-checkout obj
                        prop
                        :param-only param-only)
    (occ-message "occ-gen-checkout-if-required: no value for prop %s present for %s"
                 prop
                 (occ-Format obj))))


(cl-defmethod occ-gen-checkouts-if-required ((obj null)
                                             &key param-only)
  nil)

(cl-defmethod occ-gen-checkouts-if-required ((obj occ-obj-tsk)
                                             &key param-only)
  (let* ((props        (occ-properties-to-checkout (occ-obj-tsk obj)))
         (checkout-ops (mapcar #'(lambda (prop)
                                   (occ-gen-checkout-if-required obj
                                                                 prop
                                                                 :param-only param-only))
                               props)))
    (cl-assert props)
    (remove nil
            checkout-ops)))


(cl-defmethod occ-gen-checkouts-if-required ((obj occ-obj-ctx-tsk)
                                             &key param-only)
  ;; NOTE:
  ;; will not simply call (OCC-GEN-CHECKOUTS-IF-REQUIRED ((OBJ OCC-OBJ-TSK)  &KEY PARAM-ONLY)
  ;; as number of arguments are same, so we have to change OBJ argument to (OCC-OBJ-TSK OBJ)
  ;; or
  ;; simply call (CL-CALL-NEXT-METHOD)
  (cl-call-next-method))

(cl-defmethod occ-gen-checkouts-if-required ((obj occ-obj-ctx)
                                             &key param-only)
  nil)


(cl-defmethod occ-gen-each-prop-checkouts ((obj null)
                                           &key param-only)
  nil)

(cl-defmethod occ-gen-each-prop-checkouts ((obj occ-obj-tsk)
                                           &key param-only)
  (occ-gen-checkouts-if-required obj
                                 :param-only param-only))

(cl-defmethod occ-gen-each-prop-checkouts ((obj occ-obj-ctx-tsk)
                                           &key param-only)
  (occ-gen-checkouts-if-required obj
                                 :param-only param-only))

(cl-defmethod occ-gen-each-prop-checkouts ((obj occ-obj-ctx)
                                           &key param-only)
  nil)


(defun occ-gen-each-prop-fast-checkouts (obj
                                         &key param-only)
  (occ-gen-each-prop-checkouts obj
                               :param-only param-only))


(cl-defmethod occ-gen-simple-checkouts ((obj null)
                                        &key param-only)
  nil)

(cl-defmethod occ-gen-simple-checkouts ((obj occ-obj-tsk)
                                        &key param-only)
  (list (occ-make-callable-normal :checkout
                                  (format "Checkout %s" (occ-Format obj))
                                  #'(lambda (obj)
                                      (occ-op-props-checkout obj)))))

(cl-defmethod occ-gen-simple-checkouts ((obj occ-obj-ctx-tsk)
                                        &key param-only)
  (list (occ-make-callable-normal :checkout
                                  (format "Checkout %s" (occ-Format obj))
                                  #'(lambda (obj)
                                      (occ-op-props-checkout obj)))))

(cl-defmethod occ-gen-simple-checkouts ((obj occ-obj-ctx)
                                        &key param-only)
  nil)

;;; occ-prop-gen-checkout-actions.el ends here
