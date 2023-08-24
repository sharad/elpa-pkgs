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
                                      values)
  "Built in for LIST PROP"
  (occ-debug "occ-obj-impl-require-p7 prop %s operation %s values %s is called" prop operation values)
  (message "tsk %s, operation %s prop %s values %s" (occ-obj-Format obj) operation prop
           values)
  (not (occ-obj-intf-has-p obj
                           prop
                           values)))

(cl-defmethod occ-obj-impl-require-p ((obj       occ-obj-tsk)
                                      (operation (eql put))
                                      (prop      symbol)
                                      values)
  "Built in for LIST PROP"
  (message "tsk %s, operation %s prop %s values %s" (occ-obj-Format obj) operation prop
           values)
  (ignore obj)
  (occ-debug "occ-obj-impl-require-p10 prop %s operation %s values %s is called" prop operation values)
  nil)

(cl-defmethod occ-obj-impl-require-p ((obj       occ-obj-tsk)
                                      (operation (eql remove))
                                      (prop      symbol)
                                      values)
  "Built in for LIST PROP"
  (message "tsk %s, operation %s prop %s values %s" (occ-obj-Format obj) operation prop
           values)
  (occ-debug "occ-obj-impl-require-p8 prop %s operation %s values %s is called" prop operation values)
  (occ-obj-intf-has-p obj
                      prop
                      values))

;; (cl-defmethod occ-obj-impl-require-p ((obj       occ-obj-tsk)
;;                                  (operation (eql member))
;;                                  (prop      symbol)
;;                                  values)
;;   (ignore obj)
;;   (occ-debug "occ-obj-impl-require-p9 prop %s operation %s values %s is called" prop operation values)
;;   nil)



(cl-defmethod occ-do-impl-operation ((obj       marker)
                                     (operation symbol)
                                     (prop      symbol)
                                     values)
  "Accept occ compatible VALUES"
  (occ-debug "(occ-do-impl-operation occ-obj-tsk symbol symbol): operation %s prop %s" operation prop)
  (let ((mrk (occ-obj-marker obj)))
    (let ((retval (occ-obj-org-call-operation-at-point mrk ;work in org file
                                                       prop
                                                       operation
                                                       ;; going to org world
                                                       (occ-obj-intf-to-org prop
                                                                            values))))
      (occ-debug "occ-do-impl-operation: (occ-obj-org-call-operation-at-point mrk) returnd %s" retval)
      retval)))

(cl-defmethod occ-do-impl-operation ((obj       occ-obj-tsk)
                                     (operation symbol)
                                     (prop      symbol)
                                     values)
  "Accept occ compatible VALUES"
  (occ-debug "(occ-do-impl-operation occ-obj-tsk symbol symbol): operation %s prop %s" operation prop)
  (occ-do-intf-operation (occ-obj-marker obj)
                         operation
                         prop
                         values))


;;; * few frequent operations

;; (cl-defmethod occ-do-impl-operation ((obj       occ-obj-tsk)
;;                                  (operation (eql get))
;;                                  (prop      symbol)
;;                                  values)
;;   (ignore values)
;;   (let ((tsk (occ-obj-tsk obj)))
;;       (occ-debug "(occ-do-impl-operation occ-obj-tsk): operation %s prop %s" operation prop)
;;       (if (occ-obj-intf-list-p prop)
;;           (occ-obj-get-property tsk
;;                                 prop)
;;         (list (occ-obj-get-property tsk
;;                                     prop)))))

(cl-defmethod occ-do-impl-operation ((obj       occ-obj-tsk)
                                     (operation (eql add))
                                     (prop      symbol)
                                     values)
  (let ((tsk (occ-obj-tsk obj)))
    (occ-debug "(occ-do-impl-operation occ-obj-tsk add): operation %s prop %s" operation prop)
    (if (occ-obj-intf-list-p prop)
        (occ-obj-set-property tsk prop
                              (nconc (occ-obj-get-property tsk prop)
                                     (list (cl-first values))))
      (occ-obj-set-property tsk prop
                            (cl-first values)))))

(cl-defmethod occ-do-impl-operation ((obj       occ-obj-tsk)
                                     (operation (eql put))
                                     (prop      symbol)
                                     values)
  (let ((tsk (occ-obj-tsk obj)))
    (occ-debug "(occ-do-impl-operation occ-obj-tsk): operation %s prop %s" operation prop)
    (if (occ-obj-intf-list-p prop)
        (occ-obj-set-property tsk prop
                              values)
      (occ-obj-set-property tsk prop
                            (cl-first values)))))

(cl-defmethod occ-do-impl-operation ((obj       occ-obj-tsk)
                                     (operation (eql remove))
                                     (prop      symbol)
                                     values)
  (let ((tsk (occ-obj-tsk obj)))
    (occ-debug "(occ-do-impl-operation occ-obj-tsk): operation %s prop %s" operation prop)
    (if (occ-obj-intf-list-p prop
                             (occ-obj-set-property tsk prop
                                                   (remove (cl-first values)
                                                           (occ-obj-get-property tsk prop))))
        (occ-error "Implement it."))))

(cl-defmethod occ-do-impl-operation ((obj       occ-obj-tsk)
                                     (operation (eql delete))
                                     (prop      symbol)
                                     values)
  (occ-error "Implement it."))

;; (cl-defmethod occ-do-impl-operation ((obj       occ-obj-tsk)
;;                                  (operation (eql member))
;;                                  (prop      symbol)
;;                                  values)
;;   (let ((tsk (occ-obj-tsk obj)))
;;     (occ-debug "(occ-do-impl-operation occ-obj-tsk): operation %s prop %s" operation prop)
;;     (occ-obj-intf-has-p tsk prop
;;                    values)))


(cl-defmethod occ-do-impl-operation ((obj       marker)
                                     (operation (eql add))
                                     (prop      symbol)
                                     values)
  (occ-do-org-operation marker
                        operation
                        prop
                        values))

(cl-defmethod occ-do-impl-operation ((obj       marker)
                                     (operation (eql put))
                                     (prop      symbol)
                                     values)
  (occ-do-org-operation marker
                        operation
                        prop
                        values))

(cl-defmethod occ-do-impl-operation ((obj       marker)
                                     (operation (eql remove))
                                     (prop      symbol)
                                     values)
  (occ-do-org-operation marker
                        operation
                        prop
                        values))
;;; occ-impl-builtin.el ends here
