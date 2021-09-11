;;; occ-prop-checkout.el --- property checkout code  -*- lexical-binding: t; -*-

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

(provide 'occ-prop-checkout)


(require 'occ-prop-base)


(cl-defgeneric occ-op-prop-checkout (obj
                                     prop)
  "Checkout property PROP for forced clock-in.")

(cl-defmethod occ-op-prop-checkout ((obj  occ-obj-tsk)
                                    (prop symbol))
  "Checkout property PROP for forced clock-in."
  (occ-checkout-prop obj prop))


(cl-defgeneric occ-op-props-checkout (obj)
  "Checkout all property for forced clock-in.")

(cl-defmethod occ-op-props-checkout ((obj occ-obj-tsk))
  "Checkout all property for forced clock-in."
  (dolist (prop (occ-properties-to-checkout obj))
    (occ-message "occ-checkout: checkout prop %s" prop)
    (occ-op-prop-checkout obj prop)))

;;; occ-prop-checkout.el ends here
