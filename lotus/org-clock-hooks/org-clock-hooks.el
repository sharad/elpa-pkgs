;;; org-clock-hooks.el --- org-context-clock-api               -*- lexical-binding: t; -*-

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


(provide 'org-clock-hooks)


(require 'org-misc-utils-lotus)
(require 'org-clock-daysummary)


(defvar org-clock-default-effort "1:00")


(defun lotus-org-mode-add-default-effort ()
  "Add a default effort estimation."
  (lotus-org-with-safe-modification
    (unless (org-entry-get (point) "Effort")
      (org-set-property "Effort"
                        org-clock-default-effort))))

(defun lotus-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort (completing-read "Effort: "
                                   (org-entry-get-multivalued-property (point)
                                                                       "Effort"))))
      (unless (equal effort "")
        (lotus-org-with-safe-modification
          (org-set-property "Effort" effort))))))

(defun org-add-effort-if-not-clockin-hook ()
  "if effort is not present than ask for it."
  (unless (lotus-org-unnamed-task-at-point-p)
    (when (not (and (boundp' org-timer-countdown-timer)
                    org-timer-countdown-timer))
      (if (org-entry-get nil "Effort")
          (save-excursion
            (forward-line -2)
            (org-timer-set-timer))
        (call-interactively 'org-timer-set-timer)))
    (save-buffer)))

(defun org-clock-add-schedule-on-clockin ()
  (let ((schedule (org-get-scheduled-time nil)))
    (unless schedule
      (org-schedule nil))))

(defun org-clock-add-deadline-on-clockin ()
  (let ((deadline (org-get-deadline-time nil)))
    (unless deadline
      (org-deadline nil))))

(defun org-clock-add-schedule-if-not-clockin-hook ()
  (org-clock-add-schedule-on-clockin))

(defun org-clock-add-deadline-if-not-clockin-hook ()
  (org-clock-add-deadline-on-clockin))

(defun org-clock-add-deadline-schedule-clockin-hook ()
  (unless (lotus-org-unnamed-task-at-point-p)
    (org-clock-add-schedule-if-not-clockin-hook)
    (when (org-get-scheduled-time nil)
      (org-clock-add-deadline-if-not-clockin-hook))))

;; (org-update-all-dblocks) check about it

(defun org-timer-cleanup-clockout-hook ()
  (when (and (boundp' org-timer-countdown-timer)
             org-timer-countdown-timer)
    (org-timer-stop))
  (org-work-day-get-clock-string t))
  ;; Need some way here to run after org-lognote completed
  ;; (save-buffer) ;; -- what to do ???


;;;###autoload
(defun lotus-org-clock-in/out-insinuate-hooks ()
  (add-hook 'org-clock-in-hook
            #'org-clock-add-deadline-schedule-clockin-hook)
  (add-hook 'org-clock-in-hook
            #'org-add-effort-if-not-clockin-hook)
  (add-hook 'org-clock-out-hook
            #'org-timer-cleanup-clockout-hook)
  (add-hook 'org-clock-in-prepare-hook
            'lotus-org-mode-ask-effort))


;;; org-clock-hooks.el ends here
