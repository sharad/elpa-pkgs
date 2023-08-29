;;; occ-prop-org.el --- occ prop org                 -*- lexical-binding: t; -*-

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


(provide 'occ-prop-org)


(require 'org)
(require 'org-misc-utils-lotus)

(require 'occ-prop-intf)
(require 'occ-intf)
(require 'occ-obj-common)
(require 'occ-property-methods)
(require 'occ-obj-accessor)
(require 'occ-debug-method)



(eval-when-compile
  (require 'lotus-misc-utils))


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
    (org-entry-get-multivalued-property pom
                                        prop)))

(defun occ-org-entry-put-multivalued-property (pom
                                               prop
                                               values)
  (lotus-org-with-safe-modification
    (apply #'org-entry-put-multivalued-property
           pom
           prop
           values)))

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


(cl-defgeneric occ-do-org-operation (pom
                                     operation
                                     prop
                                     value)
  "Org operation implementation of OPERATION on POINT-OF-MARKER for
PROP and VALUES")

(cl-defmethod occ-do-org-operation :around ((mrk  marker)
                                            (operation symbol)
                                            (prop symbol)
                                            value)
  "Accept org compatible VALUE"
  (occ-message "I should be called first in case of MARKER")
  ;; (unless (occ-obj-valid-p operation prop)
  ;;   (occ-error "occ-obj-org-operation[around]: operation %s(type=%s) is not allowed for prop %s(type=%s)"
  ;;              operation
  ;;              (type-of operation)
  ;;              prop
  ;;              (type-of prop)))
  (lotus-with-marker mrk
    (unless (org-get-property-block)
      ;; create property drawer
      ;; TODO: NOTE: only create property block if 100% sure value is going to be set.
      (occ-debug "occ-do-org-operation[ :around ]: property block not exist so creating it.")
      (let* ((range (org-get-property-block (point) 'force))
             (start (when (consp range) (1- (cl-first range)))))
        (if (and range
                 start)
            (when (numberp start)
              (goto-char start))
          (occ-error "occ-do-org-operation[ :around ]: not able to create property block to add property %s: %s"
                     prop
                     value))))

    (if (org-get-property-block)
        (progn
          (occ-debug "occ-do-org-operation[ :around ]: adding prop: %s value: %s using (org-set-property)."
                     prop
                     value)
          (let ((retval (cl-call-next-method)))
            (occ-debug "occ-do-org-operation: (occ-do-org-operation mrk) returned %s" retval)
            retval))
        (occ-error "occ-do-org-operation[ :around ]: can not get property block to add property %s: %s"
                   prop
                   value))))

(cl-defmethod occ-do-org-operation ((pom  marker)
                                    (operation symbol)
                                    (prop symbol)
                                    value)
  "Org operation implementation of OPERATION on POINT-OF-MARKER
for prop REMOVE and VALUES"
  (ignore operation)
  (occ-do-intf-operation pom
                         operation
                         prop
                         value))

(cl-defmethod occ-do-org-operation ((pom  marker)
                                    (operation (eql get))
                                    (prop symbol)
                                    value)
  "Org operation implementation of OPERATION on POINT-OF-MARKER for
prop GET and VALUES"
  (ignore operation)
  (ignore value)
  (let ((prop-string (symbol-name prop)))
      (if (occ-obj-intf-list-p prop)
          (occ-org-entry-get-multivalued-property pom
                                                    prop-string)
        (list (occ-org-entry-get pom
                                 prop-string)))))

(cl-defmethod occ-do-org-operation ((pom  marker)
                                    (operation (eql add))
                                    (prop symbol)
                                    value)
  "Org operation implementation of OPERATION on POINT-OF-MARKER for
prop ADD and VALUES"
  (ignore operation)
  (let ((prop-string (symbol-name prop)))
      (if (occ-obj-intf-list-p prop)
          (occ-org-entry-add-to-multivalued-property pom
                                                     prop-string
                                                     value)
        (occ-error "Property `%s' is type of LIST, %s operation not applied to it." prop (upcase (symbol-name operation))))))

(cl-defmethod occ-do-org-operation ((pom  marker)
                                    (operation (eql put))
                                    (prop symbol)
                                    value)
  "Org operation implementation of OPERATION on POINT-OF-MARKER
for prop PUT and VALUES"
  (ignore operation)
  (let ((prop-string (symbol-name prop)))
    (occ-org-entry-put-multivalued-property pom
                                            prop-string
                                            value)))

(cl-defmethod occ-do-org-operation ((pom  marker)
                                    (operation (eql remove))
                                    (prop symbol)
                                    value)
  "Org operation implementation of OPERATION on POINT-OF-MARKER
for prop REMOVE and VALUES"
  (ignore operation)
  (let ((prop-string (symbol-name prop)))
      (if (occ-obj-intf-list-p prop)
          (occ-org-entry-remove-from-multivalued-property pom
                                                          prop-string
                                                          values)
        (occ-error "Property `%s' is type of LIST, %s operation not applied to it." prop (upcase (symbol-name operation))))))

(cl-defmethod occ-do-org-operation ((pom  marker)
                                    (operation (eql delete))
                                    (prop symbol)
                                    value)
  "Org operation implementation of OPERATION on POINT-OF-MARKER
for prop REMOVE and VALUES"
  (ignore operation)
  (let ((prop-string (symbol-name prop)))
    (occ-org-entry-delete pom
                          prop-string
                          value)))

(cl-defmethod occ-do-org-operation ((pom  marker)
                                    (operation (eql member))
                                    (prop symbol)
                                    value)
  "Org operation implementation of OPERATION on POINT-OF-MARKER
for prop MEMBER and VALUES"
  (ignore operation)
  (let ((prop-string (symbol-name prop)))
      (if (occ-obj-intf-list-p prop)
          (occ-org-entry-member-in-multivalued-property pom
                                                        prop-string
                                                        value)
        (string= value
                 (occ-obj-to-org prop
                                 operation
                                 (occ-org-entry-get pom
                                                    prop-string))))))

(cl-defmethod occ-do-org-operation ((obj occ-obj-tsk)
                                    (operation symbol)
                                    (prop symbol)
                                    value)
  ;; NOTE: Not used
  "Org operation implementation of OPERATION on POINT-OF-MARKER for
PROP and VALUES"
  (occ-do-org-operation (occ-obj-marker obj)
                        operation
                        prop
                        (occ-obj-to-org prop
                                        operation
                                        value)))


(cl-defmethod occ-do-readprop-org ((obj  occ-obj-ctx-tsk)
                                   (prop symbol))
  "Read property PROP of OBJ-CTX-TSK OBJ from its corresponding org file entry."
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (ignore ctx)
    (let* ((mrk    (or (occ-obj-marker tsk) (point)))
           (values (occ-do-org-operation-at-point mrk
                                                  prop
                                                  'get)))
      (mapcar #'(lambda (v)
                  (occ-obj-from-org prop
                                    nil
                                    v))
              values))))

(cl-defmethod occ-do-writeprop-org ((obj  occ-obj-ctx-tsk)
                                    (prop symbol))
  "Write property PROP of OBJ-CTX-TSK OBJ to its corresponding org file entry."
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (ignore ctx)
    (let* ((values (occ-obj-get-property tsk prop))
           (values (if (consp values) values (list values)))
           (values (mapcar #'(lambda (v)
                               (occ-obj-to-org prop 'put v))
                           values)))
      (occ-do-org-operation tsk
                            prop
                            'put
                            values))))

;;; occ-prop-org.el ends here
