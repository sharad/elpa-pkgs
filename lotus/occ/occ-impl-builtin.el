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

(provide 'occ-impl-buildtin)


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
  (not (occ-obj-intf-has-p obj prop
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
  (occ-obj-intf-has-p obj prop
                      values))

;; (cl-defmethod occ-obj-impl-require-p ((obj       occ-obj-tsk)
;;                                  (operation (eql member))
;;                                  (prop      symbol)
;;                                  values)
;;   (ignore obj)
;;   (occ-debug "occ-obj-impl-require-p9 prop %s operation %s values %s is called" prop operation values)
;;   nil)



;;; occ-impl-builtin.el ends here
