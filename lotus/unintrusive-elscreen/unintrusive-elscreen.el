;;; unintrusive-elscreen.el --- us                   -*- lexical-binding: t; -*-

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

(provide 'unintrusive-elscreen)


(defvar unintrusive-elscreen-display-tab-idle-sec 3 "unintrusive-elscreen-display-tab-idle-sec")
(defvar unintrusive-elscreen-display-tab-on-for-cmd-secs 3 "unintrusive-elscreen-display-tab-on-for-cmd-secs")
(defvar unintrusive-elscreen-display-tab-on-for-all-secs 3 "unintrusive-elscreen-display-tab-on-for-all-secs")
(defvar unintrusive-elscreen-display-tab-on-for-functions
  '(elscreen-next
    elscreen-previous
    elscreen-kill
    elscreen-create
    elscreen-goto
    elscreen-jump
    elscreen-swap
    elscreen-clone))
(defvar unintrusive-original-header-line-format nil)
(make-variable-buffer-local 'unintrusive-original-header-line-format)


(defun unintrusive-elscreen-display-tab-on (&optional secs)
  "Toggle the tab on the top of screen."
  (let ((secs (or secs
                  unintrusive-elscreen-display-tab-on-for-all-secs)))
    (unless elscreen-display-tab
      (setq unintrusive-original-header-line-format header-line-format)
      (setq elscreen-display-tab t)
      (if secs
          (run-at-time secs nil #'add-hook 'pre-command-hook #'unintrusive-elscreen-display-tab-off)
        (add-hook 'pre-command-hook #'unintrusive-elscreen-display-tab-off))
      (elscreen-notify-screen-modification 'force-immediately))))


(defun unintrusive-elscreen-display-tab-off ()
  "Toggle the tab on the top of screen."
  (when elscreen-display-tab
    (setq header-line-format unintrusive-original-header-line-format)
    (remove-hook 'pre-command-hook #'unintrusive-elscreen-display-tab-off)
    (setq elscreen-display-tab nil)
    (elscreen-notify-screen-modification 'force)))

(defun unintrusive-elscreen-display-tab-on-for-sometime (&rest r)
  ;; ignore r
  (unintrusive-elscreen-display-tab-on unintrusive-elscreen-display-tab-on-for-cmd-secs))

;;;###autoload
(define-minor-mode unintrusive-elscreen-display-tab-mode
  "unintrusive-elscreen-display-tab-mode"
  :init-value nil
  :global t
  (if unintrusive-elscreen-display-tab-mode
      (progn
        (dolist (f unintrusive-elscreen-display-tab-on-for-functions)
          (when (symbol-function f)
            (add-function :before (symbol-function f)
                          #'unintrusive-elscreen-display-tab-on-for-sometime)))
        (add-hook 'pre-command-hook #'unintrusive-elscreen-display-tab-off)
        (setq unintrusive-elscreen-display-tab-timer
              (run-with-idle-timer unintrusive-elscreen-display-tab-idle-sec
                                   unintrusive-elscreen-display-tab-idle-sec
                                   #'unintrusive-elscreen-display-tab-on)))
    (progn
      (dolist (f unintrusive-elscreen-display-tab-on-for-functions)
        (when (symbol-function f)
          (remove-function (symbol-function f)
                           #'unintrusive-elscreen-display-tab-on-for-sometime)))
      (remove-hook 'pre-command-hook #'unintrusive-elscreen-display-tab-off)
      (when unintrusive-elscreen-display-tab-timer
        (cancel-timer unintrusive-elscreen-display-tab-timer)
        (setq unintrusive-elscreen-display-tab-timer nil))
      (unintrusive-elscreen-display-tab-on))))

;;; unintrusive-elscreen.el ends here
