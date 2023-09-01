;;; occ-impl-builtin.el --- OCC implementation generics and general default methods  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Music Player Daemon (MPD) user

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(provide 'occ-impl-builtin)


(require 'occ-prop-org)


;; (cl-defmethod occ-obj-impl-require-p ((obj       occ-obj-tsk)
;;                                  (operation (eql get))
;;                                  (prop      symbol)
;;                                  values)
;;   ;; Required by occ-obj-gen-edit-if-required in occ-prop-gen-edit-actions.el#L101
;;   ;; To generate add and delete increment actions
;;   (ignore obj)
;;   (occ-debug "occ-obj-impl-require-p11 prop %s operation %s values %s is called" prop operation values)
;;   nil)

(cl-defmethod occ-obj-impl-require-p ((obj       occ-obj-tsk)
                                      (operation (eql add))
                                      (prop      symbol)
                                      value)
  "Built in for LIST PROP"
  (occ-debug "occ-obj-impl-require-p7 prop %s operation %s values %s is called" prop operation value)
  (message "tsk %s, operation %s prop %s values %s" (occ-obj-Format obj) operation prop
           value)
  (not (occ-obj-intf-has-p obj
                           prop
                           value)))

(cl-defmethod occ-obj-impl-require-p ((obj       occ-obj-tsk)
                                      (operation (eql put))
                                      (prop      symbol)
                                      value)
  "Built in for LIST PROP"
  (occ-message "tsk %s, operation %s prop %s values %s"
               (occ-obj-Format obj)
               operation
               prop
               value)
  (ignore obj)
  (occ-debug "occ-obj-impl-require-p10 prop %s operation %s values %s is called" prop operation value)
  (not (occ-obj-intf-has-p obj
                           prop
                           value)))

(cl-defmethod occ-obj-impl-require-p ((obj       occ-obj-tsk)
                                      (operation (eql remove))
                                      (prop      symbol)
                                      value)
  "Built in for LIST PROP"
  (occ-message "tsk %s, operation %s prop %s values %s"
               (occ-obj-Format obj)
               operation
               prop
               value)
  (occ-debug "occ-obj-impl-require-p8 prop %s operation %s values %s is called" prop operation value)
  (occ-obj-intf-has-p obj
                      prop
                      value))

(cl-defmethod occ-obj-impl-require-p ((obj       occ-obj-tsk)
                                      (operation (eql delete))
                                      (prop      symbol)
                                      value)
  "Built in for LIST PROP"
  (occ-message "tsk %s, operation %s prop %s values %s"
               (occ-obj-Format obj)
               operation
               prop
               value)
  (occ-debug "occ-obj-impl-require-p8 prop %s operation %s values %s is called" prop operation value)
  (occ-obj-intf-has-p obj
                      prop
                      value))

;; (cl-defmethod occ-obj-impl-require-p ((obj       occ-obj-tsk)
;;                                  (operation (eql member))
;;                                  (prop      symbol)
;;                                  values)
;;   (ignore obj)
;;   (occ-debug "occ-obj-impl-require-p9 prop %s operation %s values %s is called" prop operation values)
;;   nil)


;; (cl-defmethod occ-do-impl-operation ((obj       marker)
;;                                      (operation (eql add))
;;                                      (prop      symbol)
;;                                      value)
;;   (occ-do-org-operation marker
;;                         operation
;;                         prop
;;                         value))

;; (cl-defmethod occ-do-impl-operation ((obj       marker)
;;                                      (operation (eql put))
;;                                      (prop      symbol)
;;                                      value)
;;   (occ-do-org-operation marker
;;                         operation
;;                         prop
;;                         value))

;; (cl-defmethod occ-do-impl-operation ((obj       marker)
;;                                      (operation (eql remove))
;;                                      (prop      symbol)
;;                                      value)
;;   (occ-do-org-operation marker
;;                         operation
;;                         prop
;;                         value))


;; (cl-defmethod occ-do-impl-operation ((obj       occ-obj-tsk)
;;                                      (operation symbol)
;;                                      (prop      symbol)
;;                                      value)
;;   "Accept occ compatible VALUES"
;;   (occ-debug "(occ-do-impl-operation occ-obj-tsk symbol symbol): operation %s prop %s" operation prop)
;;   (occ-do-intf-operation (occ-obj-marker obj)
;;                          operation
;;                          prop
;;                          value))


;;; * few frequent operations

;; (cl-defmethod occ-do-impl-operation ((obj       occ-obj-tsk)
;;                                  (operation (eql get))
;;                                  (prop      symbol)
;;                                  values)
;;   (ignore values)
;;   (let ((tsk (occ-obj-tsk obj)))
;;       (occ-debug "(occ-do-impl-operation occ-obj-tsk): operation %s prop %s" operation prop)
;;       (if (occ-obj-list-p obj prop)
;;           (occ-obj-get-property tsk
;;                                 prop)
;;         (list (occ-obj-get-property tsk
;;                                     prop)))))

;; (cl-defmethod occ-do-impl-operation ((obj       marker)
;;                                      (operation symbol)
;;                                      (prop      symbol)
;;                                      value)
;;   "Accept occ compatible VALUES"
;;   (occ-debug "(occ-do-impl-operation occ-obj-tsk symbol symbol): operation %s prop %s" operation prop)
;;   (let ((mrk (occ-obj-marker obj)))
;;     ;; OCC-DO-ORG-OPERATION only define for OPERATION = add, remove, put, delete
;;     (let ((retval (occ-do-org-operation mrk ;work in org file
;;                                         operation
;;                                         prop
;;                                         ;; going to org world
;;                                         (occ-obj-intf-to-org prop
;;                                                              value))))
;;       (occ-debug "occ-do-impl-operation: (occ-obj-org-operation-at-point mrk) returnd %s" retval)
;;       retval)))


(cl-defmethod occ-do-impl-operation ((pom  marker)
                                     (operation (eql get))
                                     (prop symbol)
                                     value)
  "Org operation implementation of OPERATION on POINT-OF-MARKER for
prop GET and VALUES"
  (ignore operation)
  (ignore value)
  (let ((prop-string (symbol-name prop)))
      (if (occ-obj-list-p pom prop)
          (occ-org-entry-get-multivalued-property pom
                                                  prop-string)
        (list (occ-org-entry-get pom
                                 prop-string)))))

(cl-defmethod occ-do-impl-operation ((pom  marker)
                                     (operation (eql add))
                                     (prop symbol)
                                     value)
  "Org operation implementation of OPERATION on POINT-OF-MARKER for
prop ADD and VALUES"
  (ignore operation)
  (let ((prop-string (symbol-name prop)))
      (if (occ-obj-list-p pom prop)
          (occ-org-entry-add-to-multivalued-property pom
                                                     prop-string
                                                     value)
        (occ-error "Property `%s' is type of LIST, %s operation not applied to it." prop (upcase (symbol-name operation))))))

(cl-defmethod occ-do-impl-operation ((pom  marker)
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

(cl-defmethod occ-do-impl-operation ((pom  marker)
                                     (operation (eql remove))
                                     (prop symbol)
                                     value)
  "Org operation implementation of OPERATION on POINT-OF-MARKER
for prop REMOVE and VALUES"
  (ignore operation)
  (let ((prop-string (symbol-name prop)))
      (if (occ-obj-list-p pom prop)
          (occ-org-entry-remove-from-multivalued-property pom
                                                          prop-string
                                                          value)
        (occ-error "Property `%s' is type of LIST, %s operation not applied to it." prop (upcase (symbol-name operation))))))

(cl-defmethod occ-do-impl-operation ((pom  marker)
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

(cl-defmethod occ-do-impl-operation ((pom  marker)
                                     (operation (eql member))
                                     (prop symbol)
                                     value)
  "Org operation implementation of OPERATION on POINT-OF-MARKER
for prop MEMBER and VALUES"
  (ignore operation)
  (let ((prop-string (symbol-name prop)))
      (if (occ-obj-list-p pom prop)
          (occ-org-entry-member-in-multivalued-property pom
                                                        prop-string
                                                        value)
        (string= value
                 (occ-obj-to-org prop
                                 operation
                                 (occ-org-entry-get pom
                                                    prop-string))))))


(cl-defmethod occ-do-impl-operation ((obj       occ-obj-tsk)
                                     (operation (eql add))
                                     (prop      symbol)
                                     value)
  (let ((tsk (occ-obj-tsk obj)))
    (occ-debug "(occ-do-impl-operation occ-obj-tsk add): operation %s prop %s" operation prop)
    (if (occ-obj-list-p obj prop)
        (occ-obj-set-property tsk prop
                              ;; (nconc (occ-obj-get-property tsk prop)
                              ;;        (list (cl-first values)))
                              (cons value
                                    (occ-obj-get-property tsk prop)))
      (occ-error "Property `%s' is type of LIST, %s operation not applied to it." prop (upcase (symbol-name operation))))))
      ;; (occ-obj-set-property tsk prop
      ;;                       (cl-first values))

(cl-defmethod occ-do-impl-operation ((obj       occ-obj-tsk)
                                     (operation (eql put))
                                     (prop      symbol)
                                     value)
  (let ((tsk (occ-obj-tsk obj)))
    (occ-debug "(occ-do-impl-operation occ-obj-tsk): operation %s prop %s" operation prop)
    (occ-obj-set-property tsk prop
                          value)))

(cl-defmethod occ-do-impl-operation ((obj       occ-obj-tsk)
                                     (operation (eql remove))
                                     (prop      symbol)
                                     value)
  (let ((tsk (occ-obj-tsk obj)))
    (occ-debug "(occ-do-impl-operation occ-obj-tsk): operation %s prop %s" operation prop)
    (if (occ-obj-list-p obj prop)
        (occ-obj-set-property tsk prop
                              ;; (remove (cl-first values)
                              ;;         (occ-obj-get-property tsk prop))
                              (remove value
                                      (occ-obj-get-property tsk prop)))
      (occ-error "Property `%s' is type of LIST, %s operation not applied to it." prop (upcase (symbol-name operation))))))

(cl-defmethod occ-do-impl-operation ((obj       occ-obj-tsk)
                                     (operation (eql delete))
                                     (prop      symbol)
                                     value)
  (occ-obj-set-property tsk prop nil))

;; (cl-defmethod occ-do-impl-operation ((obj       occ-obj-tsk)
;;                                  (operation (eql member))
;;                                  (prop      symbol)
;;                                  values)
;;   (let ((tsk (occ-obj-tsk obj)))
;;     (occ-debug "(occ-do-impl-operation occ-obj-tsk): operation %s prop %s" operation prop)
;;     (occ-obj-intf-has-p tsk prop
;;                    values)))

;;; occ-impl-builtin.el ends here
