;;; occ-tsk.el --- tsk                               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(provide 'occ-tsk)


(eval-when-compile
  (require 'occ-debug-method))
(require 'occ-debug-method)


(cl-defgeneric occ-obj-build-tsks (collection)
  (ignore collection)
  (occ-error "occ-obj-build-tsks"))

(require 'occ-list-tsk)
(require 'occ-tree-tsk)

;;; occ-tsk.el ends here
