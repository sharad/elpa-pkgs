;;; occ-normalize-ineqs.el --- normalize in-equalities  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sharad

;; Author: sharad <spratap@merunetworks.com>
;; Keywords: convenience, abbrev

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

(provide 'occ-normalize-ineqs)


(require 'occ-prop-intf)


(defvar occ-ineqs '())
(defvar occ-normalized-ineq '())

(defgeneric occ-add-ineq (operator
                          prop1
                          prop2)
  "Add relative property.")

(cl-defmethod occ-add-ineq ((operator symbol)
                            (prop1    symbol)
                            (prop2    symbol))
  "Add relative property."
  (cl-assert (memq operator '(gt lt eq)))
  (cl-pushnew (list operator prop1 prop2)
              occ-ineqs))


;; (occ-obj-properties-to-calculate-rank 'occ-tsk)  
;; (occ-obj-properties-to-calculate-rank 'occ-obj-ctx-tsk)

(defvar occ-tsk-normalized-ineq '(none 1 key 10 status 20 current-clock 40 timebeing 50 root 10 currfile 20))
(defvar occ-obj-ctx-tsk-normalized-ineq '(none 1 root 10 currfile 20))


(defun occ-ineq-sum ()
  (reduce #'+
          (mapcar #'cdr
                  (cl--plist-to-alist occ-tsk-normalized-ineq))))

(defun occ-ineq-get (prop value)
  (let ((factor (plist-get occ-tsk-normalized-ineq
                           prop)))
    (* factor
       value)))


(cl-defgeneric occ-obj-ineq-rankprop (obj prop)
  "Get normalized rank.")

(cl-defmethod occ-obj-ineq-rankprop ((obj occ-tsk) (symbol prop))
  (occ-ineq-get prop
                (occ-obj-rankprop obj prop)))

(cl-defmethod occ-obj-ineq-rankprop ((obj occ-obj-ctx-tsk) (symbol prop))
  (occ-ineq-get prop
                (occ-obj-rankprop obj prop)))

;;; occ-normalize-ineqs.el ends here
