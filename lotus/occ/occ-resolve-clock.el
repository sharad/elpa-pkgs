;;; occ-resolve-clock.el --- Occ resolve clock       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <sh4r4d@gmail.com>
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

(provide 'occ-resolve-clock)


(require 'org-rl-intf nil nil)


(defun occ-rl-clock-p (clock-marker)
  (ignore clock-marker))
(defun occ-rl-clock-clock-in (clock-marker &optional resume start-time)
  (ignore clock-marker)
  (ignore resume)
  (ignore start-time))

(defun occ-rl-clock-out (&optional switch-to-state fail-quietly at-time)
  (ignore switch-to-state)
  (ignore fail-quietly)
  (ignore at-time))
(defun occ-rl-clock-clock-out (clock-marker &optional fail-quietly at-time)
  (ignore clock-marker)
  (ignore fail-quietly)
  (ignore at-time))
(defun occ-rl-select-other-clock (clock-marker &optional target)
  (ignore clock-marker)
  (ignore target))
(defun occ-rl-capture+-helm-templates-alist (clock-marker)
  (ignore clock-marker))


;;;###autoload
(defun occ-rl-register-resolve-clock ()
  (when (featurep 'org-rl-intf)
    (org-rl-intf-register 'occ (list
                                :org-rl-clock-p                       #'occ-rl-clock-p
                                :org-rl-clock-clock-in                #'occ-rl-clock-clock-in
                                :org-rl-clock-out                     #'occ-rl-clock-out
                                :org-rl-select-other-clock            #'occ-rl-select-other-clock
                                :org-rl-capture+-helm-templates-alist #'occ-rl-capture+-helm-templates-alist))))

;;;###autoload
(defun occ-rl-unregister-resolve-clock ()
  (when (featurep 'org-rl-intf)
    (org-rl-intf-unregister 'occ)))

;;; occ-resolve-clock.el ends here
