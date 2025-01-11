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
;; (require 'elscreen)
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
  ;; (message "start get-current-func-name-debug")
  (let* ((limit 50)
         (index 4)
         (frame (backtrace-frame index)))
    ;; from what I can tell, top level function call frames
    ;; start with t and the second value is the symbol of the function
    ;; (message "b4 while")
    (while (and (not (equal t (car frame)))
                (< index limit))
      ;; (message "while loop index %d" index)
      (setq frame (backtrace-frame (cl-incf index))))
    ;; (message "completed while frame")
    (if (equal t (car frame))
        (let ((fun (second frame)))
          (if (symbolp fun)
              (symbol-name fun)
            (format "%s" fun)))
      "unknown")))

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


;;;###autoload
(defvar *sessions-unified-core-session-registerd-fns-alist* nil
  "Alist of (app-name appgetfn appsetfn) app fn accept FRAME")
;;;###autoload
(defun sessions-unified-session-register-fns (app-name storefn enablefn disablefn)
  (setcdr (assoc app-name *sessions-unified-core-session-registerd-fns-alist*)
          (list app-name storefn enablefn disablefn)))
;;;###autoload
(defun sessions-unified-session-unregister-fn (app-name getfn setfn)
  (setcdr (assoc app-name *sessions-unified-core-session-registerd-fns-alist*)
          nil))
;;;###autoload
(defun sessions-unified-session-store (session-name &optional frame)
  "Store the elscreen tab configuration."
  (interactive (list (fmsession-read-location)))
  ;; (elscreen-session-store session-name frame)
  (unless (assoc session-name *sessions-unified-frames-session*)
    (push (list session-name)
          *sessions-unified-frames-session*))
  (dolist (app-appfn *sessions-unified-core-session-registerd-fns-alist*)
    (let ((app-name     (car app-appfn))
          (app-get-func (cadr app-appfn)))
      (let ((frame-data (funcall app-get-func (or nframe
                                                  (selected-frame)))))
        (let ((app-fsession-alist (assoc session-name
                                         *sessions-unified-frames-session*)))
          (if (assoc app-name app-fsession-alist)
              (setcdr (assoc app-name
                             app-fsession-alist)
                      frame-data)
            (push (cons app-name frame-data)
                  app-fsession-alist)))))))
;;;###autoload
(defun sessions-unified-session-enable (session-name &optional frame)
  "Restore the elscreen tab configuration."
  (interactive
   (list (fmsession-read-location)))
  (if session-name
      (when (assoc session-name *sessions-unified-frames-session*)
        (dolist (app-appfn *sessions-unified-core-session-registerd-fns-alist*)
          (let ((app-name     (car app-appfn))
                (app-set-func (caddr app-appfn)))
            (session-unfiy-notify "start")
            (let ((app-fsession-alist (assoc session-name
                                             *sessions-unified-frames-session*)))
              (let ((frame-data (cdr (assoc app-name
                                            app-fsession-alist))))
                (when session-unified-debug
                  (session-unfiy-notify "Nstart: session-session %s" app-name))
                (if frame-data
                    (funcall app-set-func frame-data
                             (or nframe
                                 (selected-frame)))
                  (session-unfiy-notify "Error: frame-data %s" frame-data)))))))

    (session-unfiy-notify "Error: session-name is %s" session-name)))

(defun sessions-unified-session-disable (session-name &optional frame)
  "Restore the elscreen tab configuration."
  (interactive
   (list (fmsession-read-location)))
  (if session-name
      (when (assoc session-name *sessions-unified-frames-session*)
        (dolist (app-appfn *sessions-unified-core-session-registerd-fns-alist*)
          (let ((app-name     (car app-appfn))
                (app-set-func (caddr app-appfn)))
            (session-unfiy-notify "start")
            (let ((app-fsession-alist (assoc session-name
                                             *sessions-unified-frames-session*)))
              (let ((frame-data (cdr (assoc app-name
                                            app-fsession-alist))))
                (when session-unified-debug
                  (session-unfiy-notify "Nstart: session-session %s" app-name))
                (if frame-data
                    (funcall app-set-func frame-data
                             (or nframe
                                 (selected-frame)))
                  (session-unfiy-notify "Error: frame-data %s" frame-data)))))))

    (session-unfiy-notify "Error: session-name is %s" session-name)))


(defcustom save-all-sessions-auto-save-idle-time-interval 7
  "save all sessions auto save idle time interval"
  :group 'session)
(defvar save-all-sessions-auto-save-idle-time-interval-dynamic 7 "save all sessions auto save idle time interval dynamic.")
(defcustom save-all-sessions-auto-save-time-interval (* 20 60)
  "save all sessions auto save time interval"
  :group 'session)
(defvar save-all-sessions-auto-save-time (current-time) "save all sessions auto save time")
(defvar session-debug-on-error nil "session-debug-on-error")

;;;###autoload
(defun save-all-sessions-auto-save (&optional force)
  "Save elscreen frame, desktop, and session time to time
 restore in case of sudden emacs crash."
  (interactive "P")
  (let ((idle-time (or (current-idle-time) '(0 0 0)))
        (time-format "%a %H:%M:%S"))
    ;; (time-since-save-all-sessions-auto-save-time (float-time (time-since save-all-sessions-auto-save-time)))

    (let ((time-since-last-save (float-time (time-since save-all-sessions-auto-save-time))))
      (if (or force
              (> time-since-last-save (float-time idle-time)))
          (if (or force
                  (> time-since-last-save save-all-sessions-auto-save-time-interval))
              (if (or force
                      (and idle-time
                           ;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Save-Control.html#Auto-Save-Control
                           (> (float-time idle-time) save-all-sessions-auto-save-idle-time-interval-dynamic)))
                  (progn
                    (message  "Running session-unified-save-all-sessions-before-hook")
                    (run-hooks 'session-unified-save-all-sessions-before-hook)
                    (message  "Done session-unified-save-all-sessions-before-hook")
                    ;; (message  "XYZ")
                    ;; (message "Started to save frame desktop and session.\ncurrent time %s, idle time %d idle-time-interval left %d"
                    ;;          (format-time-string time-format save-all-sessions-auto-save-time)
                    ;;          (float-time idle-time)
                    ;;          save-all-sessions-auto-save-idle-time-interval-dynamic)
                    ;; (message  "XYZ test1")
                    ;; (message "curr fn: %s" (get-current-func-name))
                    ;; (message  "XYZ test2")
                    (session-unfiy-notify "Started to save frame desktop and session.\ncurrent time %s, idle time %d idle-time-interval left %d"
                                          (format-time-string time-format save-all-sessions-auto-save-time)
                                          (float-time idle-time)
                                          save-all-sessions-auto-save-idle-time-interval-dynamic)
                    ;; (message  "XYZ test3")
                    (setq save-all-sessions-auto-save-time (current-time)
                          save-all-sessions-auto-save-idle-time-interval-dynamic save-all-sessions-auto-save-idle-time-interval)
                    (prog1
                        (if session-debug-on-error
                            (progn
                              (when sessions-unified-elscreen
                                (message  "Running save-all-frames-session")
                                (save-all-frames-session)
                                (message  "Done save-all-frames-session"))
                              (message  "Running session-vc-save-session")
                              (session-vc-save-session)
                              (message  "Done session-vc-save-session")
                              (when *session-unified-desktop-enabled* (my-desktop-save))
                              (session-unfiy-notify "Saved frame desktop and session.")
                              (message nil))
                          (condition-case e
                              (progn
                                (when sessions-unified-elscreen
                                  (save-all-frames-session))
                                (session-vc-save-session)
                                (when *session-unified-desktop-enabled* (my-desktop-save))
                                (session-unfiy-notify "Saved frame desktop and session.")
                                (message nil))
                            ('error
                             (progn
                               ;; make after 2 errors.
                               (session-unfiy-notify "Error: %s" e)
                               (cl-incf *my-desktop-save-error-count*)
                               (unless(< *my-desktop-save-error-count* *my-desktop-save-max-error-count*)
                                 (setq *my-desktop-save-error-count* 0)
                                 (session-unfiy-notify "Error %s" e)

                                 (lotus-disable-session-saving))))))
                      (run-hooks 'session-unified-save-all-sessions-after-hook)))
                (setq save-all-sessions-auto-save-idle-time-interval-dynamic
                      (1- save-all-sessions-auto-save-idle-time-interval-dynamic))))

        (setq save-all-sessions-auto-save-time (current-time)
              save-all-sessions-auto-save-idle-time-interval-dynamic save-all-sessions-auto-save-idle-time-interval)))))

(defun save-all-sessions-auto-save-immediately ()
  (save-all-sessions-auto-save t))

;;; session-config.el ends here
