;;; sessions-unified.el --- Sessions Unified         -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Music Player Daemon (MPD) user

;; Author: Music Player Daemon (MPD) user <spratap@merunetworks.com>
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

(provide 'sessions-unified)


;;;###autoload
(defvar *sessions-unified-core-session-registerd-restore-list* nil
  "Alist of (app-name appgetfn appsetfn) app fn accept FRAME")

;;;###autoload
(defvar *sessions-unified-core-session-registerd-store-list* nil
  "Alist of (app-name appgetfn appsetfn) app fn accept FRAME")

;; (defvar sessions-unified-desktop t)
;; (defvar sessions-unified-elscreen nil)
;; (defvar sessions-unified-centaur-tab nil)
;; (defvar sessions-unified-tab-bar nil)

;; (declare-function sessions-unified-session-store "desktop-unified" (&optional force))
;; (declare-function sessions-unified-core-session-store-immediately "desktop-unified" (&optional force))
;; (declare-function frame-session-restore-unhook-func "fmsession" ())
;; (declare-function frame-session-restore-force "fmsession" (nframe))
;; (declare-function frame-session-save "fmsession" (nframe))


;; (defvar *session-unified-desktop-enabled* t "Enable desktop restoration.")
;; (defvar *session-unified-session-enabled* t "Enable session restoration.")
(defvar *sessions-unified-core-run-enable-restore-interrupting-feature-delay-time* 10)


(defvar *lotus-desktop-session-store-max-error-count* 6 "")
(defvar *lotus-desktop-session-store-error-count* 0 "")


(defvar sessions-unified-disable-session-restore-interrupting-feature-hook nil
  "feature that need to be disabled for proper restoring of
desktop.")
(defvar sessions-unified-disable-session-restore-interrupting-feature-hook-old nil)

(defvar sessions-unified-enable-session-restore-interrupting-feature-hook nil
  "feature that were disabled for proper restoring of desktop will
get re-enabled here.")
(defvar sessions-unified-enable-session-restore-interrupting-feature-hook-old nil)

(defvar session-unified-save-all-sessions-before-hook nil "Hook run before saving all session")
(defvar session-unified-save-all-sessions-after-hook nil "Hook run after saving all session")



;;;###autoload
(defun sessions-unified-add-to-enable-session-restore-interrupting-feature-hook (fn &optional append local)
  (interactive)
  (message "sessions-unified-add-to-enable-session-restore-interrupting-feature-hook: Adding function\n%S"
           fn)
  (message "sessions-unified-add-to-enable-session-restore-interrupting-feature-hook: restore=%S != store=%S"
           *sessions-unified-core-session-registerd-restore-list*
           *sessions-unified-core-session-registerd-store-list*)
  (add-to-hook 'sessions-unified-enable-session-restore-interrupting-feature-hook
               fn
               append
               local))
;;;###autoload
(defun sessions-unified-remove-from-enable-session-restore-interrupting-feature-hook (fn &optional local)
  (interactive)
  (remove-hook 'sessions-unified-enable-session-restore-interrupting-feature-hook
               fn
               local))
;;;###autoload
(defvar *sessions-unified-run-enable-restore-interrupting-feature-run-timer* nil)
(defun sessions-unified-run-enable-restore-interrupting-feature-run ()
  "run hook"
  (interactive)
  (session-unify-notify "sessions-unified-run-enable-restore-interrupting-feature-run: Enabled session saving")
  (session-unify-notify "sessions-unified-run-enable-restore-interrupting-feature-run: running sessions-unified-enable-session-restore-interrupting-feature-hook hook now.")
  (when *sessions-unified-run-enable-restore-interrupting-feature-run-timer*
    (cancel-timer *sessions-unified-run-enable-restore-interrupting-feature-run-timer*))
  (setq *sessions-unified-run-enable-restore-interrupting-feature-run-timer* nil)

  (message "sessions-unified-run-enable-restore-interrupting-feature-run: running sessions-unified-enable-session-restore-interrupting-feature-hook - %S"
           sessions-unified-enable-session-restore-interrupting-feature-hook)

  (if sessions-unified-enable-session-restore-interrupting-feature-hook
      (progn
        (run-each-hooks 'sessions-unified-enable-session-restore-interrupting-feature-hook)
        (setq sessions-unified-enable-session-restore-interrupting-feature-hook-old sessions-unified-enable-session-restore-interrupting-feature-hook)
        (setq sessions-unified-enable-session-restore-interrupting-feature-hook nil))
    (session-unify-notify "already triggered")))
;;;###autoload
(defun sessions-unified-delay-run-enable-restore-interrupting-feature (&optional secs)
  (interactive "nsecs: ")
  (session-unify-notify "scheduled sessions-unified-run-enable-restore-interrupting-feature-run to run after sometime.")
  (let* ((secs (or secs 10))
         (secs-idle secs))
    (session-unify-notify "Setting timer time %d" secs-idle)
    (message "sessions-unified-delay-run-enable-restore-interrupting-feature: will be running sessions-unified-enable-session-restore-interrupting-feature-hook - %S"
             sessions-unified-enable-session-restore-interrupting-feature-hook)
    (setq *sessions-unified-run-enable-restore-interrupting-feature-run-timer*
          (run-with-idle-timer secs-idle nil
                               #'sessions-unified-run-enable-restore-interrupting-feature-run))))
(defun sessions-unified-run-enable-restore-interrupting-feature-run-info ()
  (interactive)
  (if *sessions-unified-run-enable-restore-interrupting-feature-run-timer*
      (let* ((type    (timer--idle-delay *sessions-unified-run-enable-restore-interrupting-feature-run-timer*))
             (timesec (if *sessions-unified-run-enable-restore-interrupting-feature-run-timer*
                          (if type
                              (float-time (timer--time *sessions-unified-run-enable-restore-interrupting-feature-run-timer*))
                            (- (float-time (current-time))
                               (float-time (timer--time *sessions-unified-run-enable-restore-interrupting-feature-run-timer*))))
                        0)))
        (session-unify-notify "hooks will run %sly after %d" (or type "definite") timesec))
    (session-unify-notify "No timer present")))


;;;###autoload
(defun sessions-unified-add-to-disable-session-restore-interrupting-feature-hook (fn &optional append local)
  (interactive)
  (add-to-hook 'sessions-unified-disable-session-restore-interrupting-feature-hook
               fn
               append
               local))

;;;###autoload
(defun sessions-unified-remove-from-disable-session-restore-interrupting-feature-hook (fn &optional local)
  (interactive)
  (remove-hook 'sessions-unified-disable-session-restore-interrupting-feature-hook
               fn
               local))
;;;###autoload
(defun sessions-unified-run-disable-restore-interrupting-feature-run ()
  "run hook"
  (interactive)
  (session-unify-notify "running sessions-unified-disable-session-restore-interrupting-feature-hook hook now.")
  (run-each-hooks 'sessions-unified-disable-session-restore-interrupting-feature-hook)
  (setq sessions-unified-disable-session-restore-interrupting-feature-hook-old sessions-unified-disable-session-restore-interrupting-feature-hook)
  (setq sessions-unified-disable-session-restore-interrupting-feature-hook nil))

(defun lotus-show-hook-member (fn hook &optional message)
  (funcall (if message
               #'message
             #'format)
           "%s %s is present in %s"
           (if (or (member fn (symbol-value hook))
                   (member (symbol-function fn) (symbol-value hook)))
               "Yes"
             "No")
           fn
           hook))

;;; sessions-unified.el ends here
