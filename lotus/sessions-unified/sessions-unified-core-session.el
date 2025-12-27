;;; sessions-unified-core-session.el --- session setting  -*- lexical-binding: t; -*-

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

(provide 'sessions-unified-core-session)


(require 'rcs-backup)
(require 'cl)

(eval-when-compile
  '(require 'cl))

(require 'desktop)
(require 'session)
;; (require 'elscreen)
(require 'basic-utils)
(require 'emacs-panel)


(require 'sessions-unified)
(require 'sessions-unified-core-common)
(require 'sessions-unified-desktop)
(require 'sessions-unified-session)
(require 'sessions-unified-fsession)


(cl-defmethod sessions-unified--session-store ((app null))
  (dolist (sym *sessions-unified-core-session-registerd-store-list*)
    (sessions-unified--session-store sym)))
;; (cl-defmethod sessions-unified--session-restore ((app null))
;;   (dolist (sym *sessions-unified-core-session-registerd-store-list*)
;;     (when sym
;;       (condition-case e
;;           (sessions-unified--session-restore sym)
;;         ('error (message "Error: %s" e)))))
;;   (sessions-unified--session-enable app)
;;   t)
(cl-defmethod sessions-unified--session-restore ((app null))
  (dolist (sym *sessions-unified-core-session-registerd-restore-list*)
    (sessions-unified--session-restore sym))
  (sessions-unified--session-enable app)
  t)
(cl-defmethod sessions-unified--session-enable ((app null))
  (add-hook 'auto-save-hook #'sessions-unified-core-session-store-on-idle-interval)
  (add-hook 'kill-emacs-hook #'sessions-unified-core-session-store-immediately)
  (dolist (sym *sessions-unified-core-session-registerd-restore-list*)
    (sessions-unified--session-enable sym)))
(cl-defmethod sessions-unified--session-disable ((app null))
  (remove-hook 'auto-save-hook #'sessions-unified-core-session-store-on-idle-interval)
  (remove-hook 'kill-emacs-hook #'sessions-unified-core-session-store-immediately)
  (dolist (sym *sessions-unified-core-session-registerd-store-list*)
    (sessions-unified--session-disable sym)))
(cl-defmethod sessions-unified--session-enable ((app symbol))
  (message "sessions-unified--session-enable: General Symbol method: app=%s" app)
  (unless (member app
                  *sessions-unified-core-session-registerd-store-list*)
    (push app
          *sessions-unified-core-session-registerd-store-list*)))
(cl-defmethod sessions-unified--session-disable ((app symbol))
  (message "sessions-unified--session-disable: app=%s" app)
  (when (member app
                *sessions-unified-core-session-registerd-store-list*)
    (setq *sessions-unified-core-session-registerd-store-list* (delete app
                                                                       *sessions-unified-core-session-registerd-store-list*))))
(cl-defmethod sessions-unified--session-enable :after (app)
  (message "sessions-unified--session-enable: Called AFTER method")
  (message "sessions-unified--session-enable: may run sessions-unified-enable-session-restore-interrupting-feature-hook - %S"
           sessions-unified-enable-session-restore-interrupting-feature-hook)
  (if (= (length *sessions-unified-core-session-registerd-restore-list*)
         (length *sessions-unified-core-session-registerd-store-list*))
      (progn
        (session-unify-notify "running sessions-unified-run-enable-restore-interrupting-feature-run after %d seconds idleness"
                              *sessions-unified-core-run-enable-restore-interrupting-feature-delay-time*)
        (message "sessions-unified--session-enable: will be running sessions-unified-enable-session-restore-interrupting-feature-hook - %S"
                 sessions-unified-enable-session-restore-interrupting-feature-hook)
        (sessions-unified-delay-run-enable-restore-interrupting-feature *sessions-unified-core-run-enable-restore-interrupting-feature-delay-time*))
    (message "Not running sessions-unified-enable-session-restore-interrupting-feature-hook as restore=%S != store=%S"
             *sessions-unified-core-session-registerd-restore-list*
             *sessions-unified-core-session-registerd-store-list*)))
(cl-defmethod sessions-unified--session-disable :after (app)
  (message "sessions-unified--session-disable: may run sessions-unified-disable-session-restore-interrupting-feature-hook - %S"
           sessions-unified-disable-session-restore-interrupting-feature-hook)
  (when (= 0
           (length *sessions-unified-core-session-registerd-store-list*))
    (sessions-unified-run-disable-restore-interrupting-feature-run)))
(cl-defmethod sessions-unified--session-check (app)
  (if (called-interactively-p 'interactive)
      (progn
        (lotus-show-hook-member 'sessions-unified-core-session-store-on-idle-interval 'auto-save-hook)
        (lotus-show-hook-member 'sessions-unified-core-session-store-immediately 'kill-emacs-hook))
    (and (member #'sessions-unified-core-session-store-on-idle-interval
                 auto-save-hook)
         (member #'sessions-unified-core-session-store-immediately
                 kill-emacs-hook)))
  (dolist (sym *sessions-unified-core-session-registerd-store-list*)
    (sessions-unified--session-check sym)))

;;;###autoload
(defun sessions-unified-session-store ()
  (interactive "P")
  (prog1
      (if session-debug-on-error
          (sessions-unified--session-store nil)
        (condition-case e
            (sessions-unified--session-store nil)
          ('error
           (progn
             ;; make after 2 errors.
             (session-unify-notify "Error: %s" e)
             (cl-incf *lotus-desktop-session-store-error-count*)
             (unless(< *lotus-desktop-session-store-error-count* *lotus-desktop-session-store-max-error-count*)
               (setq *lotus-desktop-session-store-error-count* 0)
               (session-unify-notify "Error %s" e)
               ;; (lotus-disable-session-saving)
               (sessions-unified--session-disable nil))))))
    (run-hooks 'session-unified-save-all-sessions-after-hook)))
;;;###autoload
(defun sessions-unified-session-restore-all ()
  (interactive)
  (let ((show-error (called-interactively-p 'interactive)))
    (if show-error
        (unless (sessions-unified--session-restore nil)
          (progn
            (session-unify-notify "desktop loading failed :( [show-error=%s]" show-error)
            (run-at-time "1 sec" nil #'(lambda () (insert "sessions-unified-session-restore")))
            (execute-extended-command nil)
            nil))
      (condition-case e
          (if (sessions-unified--session-restore nil)
              (progn
                (session-unify-notify "desktop loaded successfully :) [show-error=%s]" show-error)
                t)
            (progn
              (session-unify-notify "desktop loading failed :( [show-error=%s]" show-error)
              nil))
        ('error
         (session-unify-notify "Error in desktop-read: %s\n not adding save-all-sessions-auto-save to auto-save-hook" e)
         (session-unify-notify "Error in desktop-read: %s try it again by running M-x sessions-unified-session-restore" e)
         (run-at-time "1 sec" nil #'(lambda () (insert "sessions-unified-session-restore")))
         (condition-case e
             (execute-extended-command nil)
           ('error (message "M-x sessions-unified-session-restore %s" e))))))))
;;;###autoload
(defun sessions-unified-session-restore ()
  (interactive)
  (let ((show-error (called-interactively-p 'interactive)))
    (if t ;; show-error
        (unless (sessions-unified--session-restore nil)
          (progn
            (session-unify-notify "desktop loading failed :( [show-error=%s]" show-error)
            (run-at-time "1 sec" nil #'(lambda () (insert "sessions-unified-session-restore")))
            (execute-extended-command nil)
            nil))
      (condition-case e
          (if (sessions-unified--session-restore nil)
              (progn
                (session-unify-notify "desktop loaded successfully :) [show-error=%s]" show-error)
                t)
            (progn
              (session-unify-notify "desktop loading failed :( [show-error=%s]" show-error)
              nil))
        ('error
         (session-unify-notify "Error in desktop-read: %s\n not adding save-all-sessions-auto-save to auto-save-hook" e)
         (session-unify-notify "Error in desktop-read: %s try it again by running M-x sessions-unified-session-restore" e)
         (run-at-time "1 sec" nil #'(lambda () (insert "sessions-unified-session-restore")))
         (condition-case e
             (execute-extended-command nil)
           ('error (message "M-x sessions-unified-session-restore %s" e))))))))
;;;###autoload
(defun sessions-unified-session-enable ()
  (interactive)
  (sessions-unified--session-enable nil))
;;;###autoload
(defalias 'lotus-enable-session-saving #'sessions-unified-session-enable)
;;;###autoload
(defun sessions-unified-session-disable ()
  (interactive)
  (sessions-unified--session-disable nil))
;;;###autoload
(defalias 'lotus-disable-session-saving #'sessions-unified-session-disable)
;;;###autoload
(defun sessions-unified-core-session-check ()
  (interactive)
  (sessions-unified--session-check nil))
;;;###autoload
(defalias 'lotus-check-session-saving #'sessions-unified-core-session-check)


(defcustom sessions-unified-core-session-store-idle-time-interval 7
  "save all sessions auto save idle time interval"
  :group 'session)
(defvar sessions-unified-core-session-store-idle-time-interval-dynamic 7 "save all sessions auto save idle time interval dynamic.")
(defcustom sessions-unified-core-session-store-time-interval (* 20 60)
  "save all sessions auto save time interval"
  :group 'session)
(defvar sessions-unified-core-session-store-time (current-time) "save all sessions auto save time")
(defvar session-debug-on-error nil "session-debug-on-error")
;;;###autoload
(defun sessions-unified-core-session-store-on-idle-interval (&optional force)
  "Save elscreen frame, desktop, and session time to time
  restore in case of sudden emacs crash."
  (interactive "P")
  (let ((idle-time (or (current-idle-time) '(0 0 0)))
        (time-format "%a %H:%M:%S"))
    ;; (time-since-sessions-unified-core-session-store-time (float-time (time-since sessions-unified-core-session-store-time)))
    (let ((time-since-last-save (float-time (time-since sessions-unified-core-session-store-time))))
      (if (or force
              (> time-since-last-save (float-time idle-time)))
          (if (or force
                  (> time-since-last-save sessions-unified-core-session-store-time-interval))
              (if (or force
                      (and idle-time
                           ;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Save-Control.html#Auto-Save-Control
                           (> (float-time idle-time) sessions-unified-core-session-store-idle-time-interval-dynamic)))
                  (progn
                    (message  "Running session-unified-save-all-sessions-before-hook")
                    (run-hooks 'session-unified-save-all-sessions-before-hook)
                    (message  "Done session-unified-save-all-sessions-before-hook")
                    (session-unify-notify "Started to save frame desktop and session.\ncurrent time %s, idle time %d idle-time-interval left %d"
                                          (format-time-string time-format sessions-unified-core-session-store-time)
                                          (float-time idle-time)
                                          sessions-unified-core-session-store-idle-time-interval-dynamic)
                    ;; (message  "XYZ test3")
                    (setq sessions-unified-core-session-store-time (current-time)
                          sessions-unified-core-session-store-idle-time-interval-dynamic sessions-unified-core-session-store-idle-time-interval)
                    (sessions-unified-session-store))
                (setq sessions-unified-core-session-store-idle-time-interval-dynamic
                      (1- sessions-unified-core-session-store-idle-time-interval-dynamic))))

        (setq sessions-unified-core-session-store-time (current-time)
              sessions-unified-core-session-store-idle-time-interval-dynamic sessions-unified-core-session-store-idle-time-interval)))))
;;;###autoload
(defalias 'save-all-sessions-auto-save #'sessions-unified-core-session-store-on-idle-interval)
(defun sessions-unified-core-session-store-immediately ()
  (sessions-unified-core-session-store-on-idle-interval t))
(defalias 'save-all-sessions-auto-save-immediately #'sessions-unified-core-session-store-immediately)

;;; sessions-unified-core-session.el ends here
