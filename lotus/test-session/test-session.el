;;; org-context-clock.el --- org-context-clock               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

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


(ert-deftest test-task-run-associated-clock ()
  (should
   (progn
     (org-context-clock-insinuate))   )
  (should (progn)))

;; (ert-deftest )


(provide 'test-session)
;;; org-context-clock.el ends here
