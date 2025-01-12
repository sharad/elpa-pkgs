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

(declare-function sessions-unified-core-session-store "desktop-unified" (&optional force))
(declare-function sessions-unified-core-session-store-immediately "desktop-unified" (&optional force))
(declare-function frame-session-restore-unhook-func "fmsession" ())
(declare-function frame-session-restore-force "fmsession" (nframe))
(declare-function frame-session-save "fmsession" (nframe))


(defvar *session-unified-desktop-enabled* t "Enable desktop restoration.")
(defvar *session-unified-session-enabled* t "Enable session restoration.")
(defvar *sessions-unified-core-run-enable-restore-interrupting-feature-delay-time* 10)


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
(defun sessions-unified-add-to-enable-session-restore-interrupting-feature-hook (fn &optional append local)
  (interactive)
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
  (session-unfiy-notify "Enabled session saving")
  (session-unfiy-notify "running sessions-unified-enable-session-restore-interrupting-feature-hook hook now.")
  (when *sessions-unified-run-enable-restore-interrupting-feature-run-timer*
    (cancel-timer *sessions-unified-run-enable-restore-interrupting-feature-run-timer*))
  (setq *sessions-unified-run-enable-restore-interrupting-feature-run-timer* nil)
  (if sessions-unified-enable-session-restore-interrupting-feature-hook
      (progn
        (run-each-hooks 'sessions-unified-enable-session-restore-interrupting-feature-hook)
        (setq sessions-unified-enable-session-restore-interrupting-feature-hook-old sessions-unified-enable-session-restore-interrupting-feature-hook)
        (setq sessions-unified-enable-session-restore-interrupting-feature-hook nil))
    (session-unfiy-notify "already triggered")))
;;;###autoload
(defun sessions-unified-delay-run-enable-restore-interrupting-feature (&optional secs)
  (interactive "nsecs: ")
  (session-unfiy-notify "scheduled sessions-unified-run-enable-restore-interrupting-feature-run to run after sometime.")
  (let* (;; (idle-time (current-idle-time))
         (secs (or secs 10))
         ;; (secs-idle (+ (if idle-time (float-time idle-time) 0) secs))
         (secs-idle secs))
    (session-unfiy-notify "Setting timer time %d" secs-idle)
    (setq *sessions-unified-run-enable-restore-interrupting-feature-run-timer*
          ;; (run-with-timer secs-idle nil #'sessions-unified-run-enable-restore-interrupting-feature-run)
          (run-with-idle-timer secs-idle nil #'sessions-unified-run-enable-restore-interrupting-feature-run))))
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
        (session-unfiy-notify "hooks will run %sly after %d" (or type "definite") timesec))
    (session-unfiy-notify "No timer present")))


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
  (session-unfiy-notify "running sessions-unified-disable-session-restore-interrupting-feature-hook hook now.")
  ;; (lotus-disable-session-saving-immediately)
  (run-each-hooks 'sessions-unified-disable-session-restore-interrupting-feature-hook)
  (setq sessions-unified-disable-session-restore-interrupting-feature-hook-old sessions-unified-disable-session-restore-interrupting-feature-hook)
  (setq sessions-unified-disable-session-restore-interrupting-feature-hook nil))


;;;###autoload
(defun lotus-disable-session-saving-immediately ()
  (interactive)
  (remove-hook 'auto-save-hook #'sessions-unified-core-session-store-on-idle-interval)
  (remove-hook 'kill-emacs-hook #'sessions-unified-core-session-store-immediately)
  (when sessions-unified-elscreen
    (frame-session-restore-unhook-func))
  (sessions-unified-run-disable-restore-interrupting-feature-run)
  (session-unfiy-notify "Removed sessions-unified-core-session-store from auto-save-hook and kill-emacs-hook"))

;;;###autoload
(defun lotus-disable-session-saving ()
  (lotus-disable-session-saving-immediately)
  (sessions-unified-core-session-disable))


;;;###autoload
(defun lotus-enable-session-saving ()
  (let ((session-unified-desktop-buffs-len (length desktop-buffer-args-list)))
    (if (or (eq desktop-restore-eager t)
            ;; (null (lotus-desktop-saved-session))
            (= session-unified-desktop-buffs-len 0))
        (lotus-enable-session-saving-immediately))
    (sessions-unified-core-session-enable)))

;;;###autoload
(defun lotus-enable-session-saving-immediately ()
  (interactive)
  (session-unfiy-notify "enter")
  (add-hook 'auto-save-hook #'sessions-unified-core-session-store-on-idle-interval)
  (add-hook 'kill-emacs-hook #'sessions-unified-core-session-store-immediately)
  (when sessions-unified-elscreen
    (ignore-errors (frame-session-restore-hook-func)))
  (progn
    (session-unfiy-notify "running sessions-unified-run-enable-restore-interrupting-feature-run after %d seconds idleness"
                          *sessions-unified-core-run-enable-restore-interrupting-feature-delay-time*)
    (sessions-unified-delay-run-enable-restore-interrupting-feature *sessions-unified-core-run-enable-restore-interrupting-feature-delay-time*))
  (session-unfiy-notify "Added sessions-unified-core-session-store to auto-save-hook and kill-emacs-hook")
  (sessions-unified-run-enable-restore-interrupting-feature-run-info)
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
(defvar *sessions-unified-core-session-registerd-fns-alist* nil
  "Alist of (app-name appgetfn appsetfn) app fn accept FRAME")

;;;###autoload
(defun sessions-unified-core-session-check ()
  (interactive)
  (if (called-interactively-p 'interactive)
      (progn
        (lotus-show-hook-member 'sessions-unified-core-session-store-on-idle-interval 'auto-save-hook)
        (lotus-show-hook-member 'sessions-unified-core-session-store-immediately 'kill-emacs-hook))
    (and (member #'sessions-unified-core-session-store-on-idle-interval
                   auto-save-hook)
         (member #'sessions-unified-core-session-store-immediately
                   kill-emacs-hook)))
  (dolist (app-appfn *sessions-unified-core-session-registerd-fns-alist*)
    (let ((app-name (car app-appfn))
          (app-func (nth 5 app-appfn)))
      (if app-func
          (funcall app-func)
        (message "For app %s no app-fun present" app-name)))))

(defalias 'lotus-check-session-saving #'sessions-unified-core-session-check)


;; (defun sessions-unified-sort (alist)
;;   (sort alist
;;         #'(lambda (x y)
;;             (< (cdr x)
;;                (cdr y)))))

(defun sessions-unified-sort (alist)
  alist)

(cl-defgeneric sessions-unified-session-store (app))
(cl-defgeneric sessions-unified-session-restore (app))
(cl-defgeneric sessions-unified-session-enable (app))
(cl-defgeneric sessions-unified-session-disable (app))
(cl-defgeneric sessions-unified-session-check (app))


(cl-defmethod sessions-unified-session-store (app)
  (dolist (sym (mapcar #'car
                         (sessions-unified-sort *sessions-unified-core-session-registerd-fns-alist*)))
    (sessions-unified-session-store sym)))
;; (cl-defmethod sessions-unified-session-restore (app alist)
;;   (let ((sym (car (or alist
;;                       (sessions-unified-sort *sessions-unified-core-session-registerd-fns-alist*)))))
;;     (sessions-unified-session-restore sym (cdr alist))))
(cl-defmethod sessions-unified-session-restore (app)
  (dolist (sym (mapcar #'car
                         (sessions-unified-sort *sessions-unified-core-session-registerd-fns-alist*w)))
    (sessions-unified-session-restore sym)))
(cl-defmethod sessions-unified-session-enable (app)
  (dolist (sym (mapcar #'car
                         (sessions-unified-sort *sessions-unified-core-session-registerd-fns-alist*)))
    (sessions-unified-session-enable sym)))
(cl-defmethod sessions-unified-session-disable (app)
  (dolist (sym (mapcar #'car
                         (sessions-unified-sort *sessions-unified-core-session-registerd-fns-alist*)))
    (sessions-unified-session-disable sym)))
(cl-defmethod sessions-unified-session-check (app)
  (dolist (sym (mapcar #'car
                         (sessions-unified-sort *sessions-unified-core-session-registerd-fns-alist*)))
    (sessions-unified-session-check sym)))


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
                    (session-unfiy-notify "Started to save frame desktop and session.\ncurrent time %s, idle time %d idle-time-interval left %d"
                                          (format-time-string time-format sessions-unified-core-session-store-time)
                                          (float-time idle-time)
                                          sessions-unified-core-session-store-idle-time-interval-dynamic)
                    ;; (message  "XYZ test3")
                    (setq sessions-unified-core-session-store-time (current-time)
                          sessions-unified-core-session-store-idle-time-interval-dynamic sessions-unified-core-session-store-idle-time-interval)
                    (prog1
                        (if session-debug-on-error
                            (sessions-unified-core-session-store)
                          (condition-case e
                              (sessions-unified-core-session-store)
                            ('error
                              (progn
                                ;; make after 2 errors.
                                (session-unfiy-notify "Error: %s" e)
                                (cl-incf *lotus-desktop-session-store-error-count*)
                                (unless(< *lotus-desktop-session-store-error-count* *lotus-desktop-session-store-max-error-count*)
                                  (setq *lotus-desktop-session-store-error-count* 0)
                                  (session-unfiy-notify "Error %s" e)

                                  (lotus-disable-session-saving))))))
                      (run-hooks 'session-unified-save-all-sessions-after-hook)))
                (setq sessions-unified-core-session-store-idle-time-interval-dynamic
                      (1- sessions-unified-core-session-store-idle-time-interval-dynamic))))

        (setq sessions-unified-core-session-store-time (current-time)
              sessions-unified-core-session-store-idle-time-interval-dynamic sessions-unified-core-session-store-idle-time-interval)))))

(defalias 'save-all-sessions-auto-save #'sessions-unified-core-session-store-on-idle-interval)
(defun sessions-unified-core-session-store-immediately ()
  (sessions-unified-core-session-store-on-idle-interval t))
(defalias 'save-all-sessions-auto-save-immediately #'sessions-unified-core-session-store-immediately)


(when nil
  (add-hook ;; 'after-init-hook
   'lotus-enable-startup-interrupting-feature-hook
   #'(lambda ()
       (run-at-time-or-now 7
                           #'sessions-unified-core-session-restore))))

;;; session-config.el ends here
