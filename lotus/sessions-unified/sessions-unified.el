;;; sessions-unified.el --- session setting

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <>
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




;; set session-mgr-utils-notify
;; run frame-session-restore-hook-func
;; run lotus-desktop-session-restore
;; in some startup-hook


;;; Code:

(provide 'sessions-unified)


(require 'rcs-backup)
(require 'cl)

(eval-when-compile
  '(require 'cl))

(require 'desktop)
(require 'session)
(require 'elscreen)
(require 'basic-utils)
(require 'emacs-panel)


(defvar sessions-unified-desktop t)
(defvar sessions-unified-elscreen nil)
(defvar sessions-unified-centaur-tab nil)
(defvar sessions-unified-tab-bar nil)

(declare-function save-all-sessions-auto-save "desktop-unified" (&optional force))
(declare-function save-all-sessions-auto-save-immediately "desktop-unified" (&optional force))
(declare-function frame-session-restore-unhook-func "fmsession" ())
(declare-function frame-session-restore-force "fmsession" (nframe))
(declare-function frame-session-save "fmsession" (nframe))


(defvar *session-unified-desktop-enabled* t "Enable desktop restoration.")
(defvar *session-unified-session-enabled* t "Enable session restoration.")
(defvar *sessions-unified-desktop-enable-restore-interrupting-feature-delay-time* 10)


(defvar lotus-disable-desktop-restore-interrupting-feature-hook nil
  "feature that need to be disabled for proper restoring of
desktop.")
(defvar lotus-disable-desktop-restore-interrupting-feature-hook-old nil)

(defvar lotus-enable-desktop-restore-interrupting-feature-hook nil
  "feature that were disabled for proper restoring of desktop will
get re-enabled here.")
(defvar lotus-enable-desktop-restore-interrupting-feature-hook-old nil)

(defvar session-unified-save-all-sessions-before-hook nil "Hook run before saving all session")
(defvar session-unified-save-all-sessions-after-hook nil "Hook run after saving all session")


(defvar *sessions-unified-utils-notify* nil)

(defun sessions-unified-utils-notify-default (title fmt &rest args)
  (message  "%s: %s" title (apply #'format fmt args)))

;; https://emacs.stackexchange.com/questions/2310/can-functions-access-their-name
(defun get-current-func-name ()
  "Get the symbol of the function this function is called from."
  ;; 5 is the magic number that makes us look
  ;; above this function
  (let* ((index 4)
         (frame (backtrace-frame index)))
    ;; from what I can tell, top level function call frames
    ;; start with t and the second value is the symbol of the function
    (while (not (equal t (cl-first frame)))
      (setq frame (backtrace-frame (cl-incf index))))
    (let ((fun (second frame)))
      (if (symbolp fun)
          (symbol-name fun)
        (format "%s" fun)))))

(defun session-unfiy-notify (fmt &rest args)
  (let ((funname (get-current-func-name)))
    ;; (message "test")
    (let ((notify (or *sessions-unified-utils-notify*
                      #'sessions-unified-utils-notify-default)))
      (unless (eq notify
                  #'sessions-unified-utils-notify-default)
        (apply #'sessions-unified-utils-notify-default funname fmt args))
      (apply notify funname fmt args))))

;; (session-unfiy-notify "Enabled session saving")
;; (apply *sessions-unified-utils-notify* "test" "fmt" '())


;;;###autoload
(defun protable-display-graphic-p ()
  (if (< emacs-major-version 24)
      (eq (frame-parameter (selected-frame) 'window-system)
          'x)
    (display-graphic-p)))


;;;###autoload
(defun add-to-enable-desktop-restore-interrupting-feature-hook (fn &optional append local)
  (interactive)
  (add-to-hook 'lotus-enable-desktop-restore-interrupting-feature-hook
               fn
               append
               local))
;;;###autoload
(defun remove-from-enable-desktop-restore-interrupting-feature-hook (fn &optional local)
  (interactive)
  (remove-hook 'lotus-enable-desktop-restore-interrupting-feature-hook
               fn
               local))
;;;###autoload
(defvar *sessions-unified-desktop-enable-restore-interrupting-feature-run-timer* nil)
(defun sessions-unified-desktop-enable-restore-interrupting-feature-run ()
  "run hook"
  (interactive)
  (session-unfiy-notify "Enabled session saving")
  (session-unfiy-notify "running lotus-enable-desktop-restore-interrupting-feature-hook hook now.")
  (when *sessions-unified-desktop-enable-restore-interrupting-feature-run-timer*
    (cancel-timer *sessions-unified-desktop-enable-restore-interrupting-feature-run-timer*))
  (setq *sessions-unified-desktop-enable-restore-interrupting-feature-run-timer* nil)
  (if lotus-enable-desktop-restore-interrupting-feature-hook
      (progn
        (run-each-hooks 'lotus-enable-desktop-restore-interrupting-feature-hook)
        (setq lotus-enable-desktop-restore-interrupting-feature-hook-old lotus-enable-desktop-restore-interrupting-feature-hook)
        (setq lotus-enable-desktop-restore-interrupting-feature-hook nil))
    (session-unfiy-notify "already triggered")))
;;;###autoload
(defun sessions-unified-desktop-enable-restore-interrupting-feature-delay-run (&optional secs)
  (interactive "nsecs: ")
  (session-unfiy-notify "scheduled sessions-unified-desktop-enable-restore-interrupting-feature-run to run after sometime.")
  (let* (;; (idle-time (current-idle-time))
         (secs (or secs 10))
         ;; (secs-idle (+ (if idle-time (float-time idle-time) 0) secs))
         (secs-idle secs))
    (session-unfiy-notify "Setting timer time %d" secs-idle)
    (setq *sessions-unified-desktop-enable-restore-interrupting-feature-run-timer*
          ;; (run-with-timer secs-idle nil #'sessions-unified-desktop-enable-restore-interrupting-feature-run)
          (run-with-idle-timer secs-idle nil #'sessions-unified-desktop-enable-restore-interrupting-feature-run))))
(defun sessions-unified-desktop-enable-restore-interrupting-feature-run-info ()
  (interactive)
  (if *sessions-unified-desktop-enable-restore-interrupting-feature-run-timer*
      (let* ((type    (timer--idle-delay *sessions-unified-desktop-enable-restore-interrupting-feature-run-timer*))
             (timesec (if *sessions-unified-desktop-enable-restore-interrupting-feature-run-timer*
                          (if type
                              (float-time (timer--time *sessions-unified-desktop-enable-restore-interrupting-feature-run-timer*))
                            (- (float-time (current-time))
                               (float-time (timer--time *sessions-unified-desktop-enable-restore-interrupting-feature-run-timer*))))
                        0)))
        (session-unfiy-notify "hooks will run %sly after %d" (or type "definite") timesec))
    (session-unfiy-notify "No timer present")))


;;;###autoload
(defun add-to-disable-desktop-restore-interrupting-feature-hook (fn &optional append local)
  (interactive)
  (when t
    (add-to-hook 'lotus-disable-desktop-restore-interrupting-feature-hook
                 fn
                 append
                 local)))
;;;###autoload
(defun remove-from-disable-desktop-restore-interrupting-feature-hook (fn &optional local)
  (interactive)
  (when t
    (remove-hook 'lotus-disable-desktop-restore-interrupting-feature-hook
                 fn
                 local)))
;;;###autoload
(defun sessions-unified-desktop-disable-restore-interrupting-feature-run ()
  "run hook"
  (interactive)
  (session-unfiy-notify "running lotus-disable-desktop-restore-interrupting-feature-hook hook now.")
  ;; (lotus-disable-session-saving-immediately)
  (run-each-hooks 'lotus-disable-desktop-restore-interrupting-feature-hook)
  (setq lotus-disable-desktop-restore-interrupting-feature-hook-old lotus-disable-desktop-restore-interrupting-feature-hook)
  (setq lotus-disable-desktop-restore-interrupting-feature-hook nil))


;;;###autoload
(defun lotus-disable-session-saving-immediately ()
  (interactive)
  (remove-hook 'auto-save-hook #'save-all-sessions-auto-save)
  (remove-hook 'kill-emacs-hook #'save-all-sessions-auto-save-immediately)
  (when sessions-unified-elscreen
    (frame-session-restore-unhook-func))
  (sessions-unified-desktop-disable-restore-interrupting-feature-run)
  (session-unfiy-notify "Removed save-all-sessions-auto-save from auto-save-hook and kill-emacs-hook"))

;;;###autoload
(defun lotus-disable-session-saving ()
  (lotus-disable-session-saving-immediately)
  (progn
    (ad-disable-advice 'desktop-idle-create-buffers 'after 'desktop-idle-complete-actions)
    (ad-update 'desktop-idle-create-buffers)
    (ad-activate 'desktop-idle-create-buffers)))


;;;###autoload
(defun lotus-enable-session-saving ()
  ;; (if (or
  ;;      (eq desktop-restore-eager t)
  ;;      (null (lotus-desktop-saved-session)))
  (let ((session-unified-desktop-buffs-len (length desktop-buffer-args-list)))
    (if (or (eq desktop-restore-eager t)
            ;; (null (lotus-desktop-saved-session))
            (= session-unified-desktop-buffs-len 0))
        (lotus-enable-session-saving-immediately)
      (progn
        (ad-enable-advice 'desktop-idle-create-buffers 'after 'desktop-idle-complete-actions)
        (ad-update 'desktop-idle-create-buffers)
        (ad-activate 'desktop-idle-create-buffers)))
    (if (lotus-desktop-saved-session)
        (message "desktop file exists.")
      (message "desktop file do not exists."))))

;;;###autoload
(defun lotus-enable-session-saving-immediately ()
  (interactive)
  (session-unfiy-notify "enter")
  (add-hook 'auto-save-hook #'save-all-sessions-auto-save)
  (add-hook 'kill-emacs-hook #'save-all-sessions-auto-save-immediately)
  (when sessions-unified-elscreen
    (ignore-errors (frame-session-restore-hook-func)))
  (progn
    (session-unfiy-notify "running sessions-unified-desktop-enable-restore-interrupting-feature-run after %d seconds idleness"
                          *sessions-unified-desktop-enable-restore-interrupting-feature-delay-time*)
    (sessions-unified-desktop-enable-restore-interrupting-feature-delay-run *sessions-unified-desktop-enable-restore-interrupting-feature-delay-time*))
  (session-unfiy-notify "Added save-all-sessions-auto-save to auto-save-hook and kill-emacs-hook")
  (sessions-unified-desktop-enable-restore-interrupting-feature-run-info)
  (session-unfiy-notify "exit"))


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

;;;###autoload
(defun lotus-check-session-saving ()
  (interactive)
  (if (called-interactively-p 'interactive)
      (progn
        (lotus-show-hook-member 'save-all-sessions-auto-save 'auto-save-hook)
        (lotus-show-hook-member 'save-all-sessions-auto-save-immediately 'kill-emacs-hook)
        (when sessions-unified-elscreen
          (lotus-show-hook-member 'frame-session-restore-force 'after-make-frame-functions)
          (lotus-show-hook-member 'frame-session-save 'delete-frame-functions)))
    (and (member #'save-all-sessions-auto-save auto-save-hook)
         (member #'save-all-sessions-auto-save-immediately kill-emacs-hook)
         (when sessions-unified-elscreen
           (member #'frame-session-restore-force after-make-frame-functions)
           (member #'frame-session-save delete-frame-functions)))))

;;; session-config.el ends here
