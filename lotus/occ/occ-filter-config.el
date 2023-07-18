;;; occ-filter-config.el --- occ filter config       -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sharad

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

(provide 'occ-filter-config)


(require 'occ-filter-op)


(defun occ-filter-config-initialize ()
  (setq occ-obj-filters nil)
  (occ-obj-build-filter :mutual-deviation "Mutual Deviation" #'occ-obj-filter-mutual-deviation)
  (occ-obj-build-filter :positive "Positive" #'occ-obj-filter-positive)
  (occ-obj-build-filter :nonnegative "Non negative" #'occ-obj-filter-nonnegative)
  (occ-obj-build-filter :identity "Identity" #'occ-obj-filter-identity)
  (occ-obj-build-filter :min "Minimum" #'occ-obj-filter-min)
  (occ-obj-build-filter :max "Maximum" #'occ-obj-filter-max))


;; Filter should be list of keys or cons of key and customized rank function
;; else occ-obj-rank will be used.

(defun occ-list-filters ()
  ;; '(:nonnegative)
  '(:identity))

;; (defun occ-match-filters ()
;;   (list :positive
;;         :mutual-deviation
;;         (list :positive #'occ-obj-member-tsk-rank)))
(defun occ-match-filters ()
  (list :identity ;; :positive
        :mutual-deviation
        (list :positive
              #'occ-obj-rank)))
;; (list :mutual-deviation #'occ-obj-member-tsk-rank)

(defun occ-never-filters ()
  "Used to filter mainly non-tsk"
  '(:nonnegative))

;;; occ-filter-config.el ends here
