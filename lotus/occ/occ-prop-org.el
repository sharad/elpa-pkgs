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
    (org-entry-get-multivalued-property pom prop)))

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
                                     values)
  "Org operation implementation of OPERATION on POINT-OF-MARKER for
PROP and VALUES")

(cl-defmethod occ-do-org-operation ((pom  marker)
                                    (operation (eql get))
                                    (prop symbol)
                                    values)
  "Org operation implementation of OPERATION on POINT-OF-MARKER for
prop GET and VALUES"
  (ignore operation)
  (ignore values)
  (let ((prop-string (symbol-name prop)))
      (if (occ-obj-list-p prop)
          (occ-org-entry-get-multivalued-property pom
                                                    prop-string)
        (list (occ-org-entry-get pom
                                 prop-string)))))

(cl-defmethod occ-do-org-operation ((pom  marker)
                                    (operation (eql add))
                                    (prop symbol)
                                    values)
  "Org operation implementation of OPERATION on POINT-OF-MARKER for
prop ADD and VALUES"
  (ignore operation)
  (let ((prop-string (symbol-name prop)))
      (if (occ-obj-list-p prop)
          (occ-org-entry-add-to-multivalued-property pom
                                                     prop-string
                                                     (cl-first values))
        (occ-org-entry-put pom
                           prop-string
                           (cl-first values)))))

(cl-defmethod occ-do-org-operation ((pom  marker)
                                    (operation (eql put))
                                    (prop symbol)
                                    values)
  "Org operation implementation of OPERATION on POINT-OF-MARKER
for prop PUT and VALUES"
  (ignore operation)
  (let ((prop-string (symbol-name prop)))
      (if (occ-obj-list-p prop)
          (occ-org-entry-put-multivalued-property pom
                                                  prop-string
                                                  values)
        (occ-org-entry-put pom
                           prop-string
                           (cl-first values)))))

(cl-defmethod occ-do-org-operation ((pom  marker)
                                    (operation (eql remove))
                                    (prop symbol)
                                    values)
  "Org operation implementation of OPERATION on POINT-OF-MARKER
for prop REMOVE and VALUES"
  (ignore operation)
  (let ((prop-string (symbol-name prop)))
      (if (occ-obj-list-p prop)
          (occ-org-entry-remove-from-multivalued-property pom
                                                          prop-string
                                                          (cl-first values))
        (occ-error "Implement it."))))

(cl-defmethod occ-do-org-operation ((pom  marker)
                                    (operation (eql member))
                                    (prop symbol)
                                    values)
  "Org operation implementation of OPERATION on POINT-OF-MARKER
for prop MEMBER and VALUES"
  (ignore operation)
  (let ((prop-string (symbol-name prop)))
      (if (occ-obj-list-p prop)
          (occ-org-entry-member-in-multivalued-property pom
                                                        prop-string
                                                        (cl-first values))
        (string= (cl-first values)
                 (occ-org-entry-get pom
                                    prop-string)))))


(cl-defmethod occ-do-org-operation ((obj occ-obj-tsk)
                                    (operation symbol)
                                    (prop symbol)
                                    values)
  "Org operation implementation of OPERATION on POINT-OF-MARKER for
PROP and VALUES"
  (occ-do-org-operation (occ-obj-marker obj)
                        operation
                        prop
                        values))


(cl-defgeneric occ-obj-org-call-operation (pom
                                           prop
                                           operation
                                           values)
  "occ-obj-org-call-operation")

(cl-defmethod occ-obj-org-call-operation ((pom  marker)
                                          (prop symbol)
                                          (operation symbol)
                                          values)
  "Accept org compatible VALUES"
  ;; (unless (occ-obj-valid-p prop operation)
  ;;   (occ-error "occ-obj-org-call-operation: operation %s is not allowed for prop %s" operation prop))
  (occ-do-org-operation pom
                        operation
                        prop
                        values))

(cl-defmethod occ-obj-org-call-operation-at-point ((mrk  marker)
                                                   (prop symbol)
                                                   operation
                                                   values)
  "Accept org compatible VALUES"
  (unless (occ-obj-valid-p operation prop)
    (occ-error "occ-obj-org-call-operation: operation %s is not allowed for prop %s" operation prop))
  (lotus-with-marker mrk
    (unless (org-get-property-block)
      ;; create property drawer
      ;; TODO: NOTE: only create property block if 100% sure value is going to be set.
      (occ-debug "occ-obj-org-call-operation-at-point: property block not exist so creating it.")
      (let* ((range (org-get-property-block (point) 'force))
             (start (when (consp range) (1- (cl-first range)))))
        (if (and range
                 start)
            (when (numberp start)
              (goto-char start))
          (occ-error "occ-obj-org-call-operation-at-point: not able to create property block to add property %s: %s"
                     prop
                     values))))

    (if (org-get-property-block)
        (progn
          (occ-debug "occ-obj-org-call-operation-at-point: adding prop: %s value: %s using (org-set-property)."
                     prop
                     values)
          (let ((retval (occ-obj-org-call-operation mrk
                                                    prop
                                                    operation
                                                    values)))
            (occ-debug "occ-obj-org-call-operation: (occ-obj-org-call-operation mrk) returned %s" retval)
            retval))
        (occ-error "occ-obj-org-call-operation-at-point: can not get property block to add property %s: %s"
                   prop
                   values))))


(cl-defmethod occ-do-readprop-org ((obj  occ-obj-ctx-tsk)
                                   (prop symbol))
  "Read property PROP of OBJ-CTX-TSK OBJ from its corresponding org file entry."
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (ignore ctx)
    (let* ((mrk    (or (occ-obj-marker tsk) (point)))
           (values (occ-obj-org-call-operation-at-point mrk
                                                        prop
                                                        'get)))
      (mapcar #'(lambda (v)
                  (occ-obj-prop-from-org prop
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
                               (occ-obj-prop-to-org prop v))
                           values)))
      (occ-obj-org-call-operation-at-point (occ-obj-marker tsk)
                                           prop
                                           'put
                                           values))))

;;; occ-prop-org.el ends here
