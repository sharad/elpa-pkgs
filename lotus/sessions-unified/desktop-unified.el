;;; desktop-unified.el --- Desktop unified           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <>
;; Keywords: convenience, internal, tools

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

(provide 'desktop-unified)


(require 'server)
(require 'lotus-utils)
(require 'emacs-panel)


(require 'sessions-unified)
(require 'session-unified)
(require 'fmsession)


(defvar lotus-read-filename-default-initial-input nil)
(defun read-file-name-timeout-if-default-input (seconds
                                                prompt
                                                &optional
                                                dir default-filename mustmatch initial predicate)
  (if lotus-read-filename-default-initial-input
      (with-timeout
          (seconds (progn
                     (when (active-minibuffer-window)
                       (abort-recursive-edit))
                     initial))
        (read-file-name prompt
                        dir
                        default-filename
                        mustmatch
                        initial predicate))
    (read-file-name prompt
                    dir
                    default-filename
                    mustmatch
                    initial
                    predicate)))

(defun read-file-name-timeout (seconds prompt &optional dir default-filename mustmatch initial predicate)
  (with-timeout
      (seconds
       (progn
         (when (active-minibuffer-window)
           (abort-recursive-edit))
         initial))
    (read-file-name prompt
                    dir
                    default-filename
                    mustmatch
                    initial
                    predicate)))


(setq desktop-io-file-version 208)

;; check function desktop-buffer-info

(with-eval-after-load "desktop"
  (defun desktop-make-create-buffer (buffer)
    (let ((l (desktop-buffer-info buffer))
          ;; (eager desktop-restore-eager)
          (eager t))
      (let ((base (pop l)))
        (when (apply 'desktop-save-buffer-p l)
          (when (and base (not (string= base "")))
            (setcar (nthcdr 1 l) base))
          `(let ((desktop-buffer-ok-count 0)
                 (desktop-buffer-fail-count 0)
                 desktop-first-buffer)
             (
              ,(if (or (not (integerp ,eager))
                       (if (zerop ,eager)
                           nil
                         (setq ,eager (1- ,eager))))
                   'desktop-create-buffer
                 'desktop-append-buffer-args)

              (string-to-number desktop-file-version)
              ;; If there's a non-empty base name, we save it instead of the buffer name
              ;; (when (and base (not (string= base "")))
              ;;   (setcar (nthcdr 1 l) base))
              ;; (dolist (e l)
              ;;   (insert "\n  " (desktop-value-to-string e)))
              ,@(mapcar #'(lambda (s)
                            (read (desktop-value-to-string s)))
                        l)))))))

  (defun desktop-make-create-buffer-list (buffer)
    (let ((l (desktop-buffer-info buffer))
          ;; (eager desktop-restore-eager)
          (eager t))
      (ignore eager)
      (let ((base (pop l)))
        (when (apply 'desktop-save-buffer-p l)
          (when (and base (not (string= base "")))
            (setcar (nthcdr 1 l) base))
          ;; `(
          ;;   (string-to-number desktop-file-version)
          ;;   ;; If there's a non-empty base name, we save it instead of the buffer name
          ;;   ;; (when (and base (not (string= base "")))
          ;;   ;;   (setcar (nthcdr 1 l) base))
          ;;   ;; (dolist (e l)
          ;;   ;;   (insert "\n  " (desktop-value-to-string e)))
          ;;   ,@(mapcar #'(lambda (s)
          ;;                (read (desktop-value-to-string s))) l))
          (cons (string-to-number desktop-file-version)
                l))))))

;; (desktop-buffer-info (current-buffer))


;; (with-eval-after-load "desktop"
;; (when session-unified-debug
;; http://stackoverflow.com/questions/2703743/restore-emacs-session-desktop
;; (desktop-save-mode 1)
;; (desktop-read)

;; from: http://www.emacswiki.org/emacs/DeskTop
;; You can add any extra variables you want saved across sessions to the list ‘desktop-globals-to-save’. For example:
;; (setq history-length 250)
;; (add-to-list 'desktop-globals-to-save 'file-name-history)

;; Specifying Files Not to be Opened

;; You can specify buffers which should not be saved, by name or by mode, e.g.:


(setq desktop-buffers-not-to-save
      (concat "\\("
              "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
              "\\)$")

      ;; check it http://stackoverflow.com/a/4055504/341107
      desktop-files-not-to-save       ;very important
      ;; default value
      ;; "\\(^/[^/:]*:\\|(ftp)$\\)"
      ;; that do not allow to save trampe file
      "^$")

(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)


;;{{
;; Automatically Overriding Stale Locks

;; If you are using desktop-mode and your emacs crashes (or more
;; likely, your system crashes), then your desktop file will not be
;; released, and emacs will bother you about using it next time you
;; start it up. To avoid this, I wrote the following code to
;; override the lock on a desktop file if the indicated process is
;; not still running.

;; desktop-override-stale-locks.el begins here
;;   (defun emacs-process-p (pid)
;;     "If pid is the process ID of an emacs process, return t, else nil.
;; Also returns nil if pid is nil."
;;     (when pid
;;       (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
;;         (when (file-exists-p cmdline-file)
;;           (with-temp-buffer
;;             (insert-file-contents-literally cmdline-file)
;;             (goto-char (point-min))
;;             (search-forward "emacs" nil t)
;;             pid)))))
(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))
;; desktop-override-stale-locks.el ends here
;; (ad-disable-advice 'desktop-owner 'after 'pry-from-cold-dead-hands)

;; If anyone has a more robust implementation of `emacs-process-p,’ feel free to provide it.

;; I implemented ‘emacs-process-p’ by following way, it could work on both Windows and Linux.

(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
so returns nil if pid is nil."
  (when pid
    (let* ((attributes (process-attributes pid))
           (cmd        (cl-rest (assoc "comm" attributes #'string=))))
      ;; (dolist (attr attributes)
      ;;   (if (string= "comm" (cl-first attr))
      ;;       (setq cmd (cl-rest attr))))
      (and cmd
           (or (string= "emacs"     cmd)
               (string= "emacs.exe" cmd)
               (string-match "^\.emacs\\(-\.+\\)$" cmd))))))
;; I think the original function contains an error. Should it not end something like:

;; (when (search-forward "emacs" nil t)
;;   pid))))))
;;}}

;;{{
;; Minimal Setup

;; This is for people who only want minimal session management
;; functionality, and don’t want their previous sessions automatically
;; restored at start-up. Note that you need desktop-save-mode NOT
;; ENABLED for this to work as intended.

;; It works for me with one desktop, with more than one may need some tweaking.

;; use only one desktop
;; (make-directory (expand-file-name "autoconfig/desktop/" user-emacs-directory) t)
;; (setq todoo-file-name (expand-file-name "autoconfig/desktop/" user-emacs-directory))

(setq desktop-path (expand-file-name "desktop/"
                                     session-unified-dir))

(setq desktop-dirname (expand-file-name "desktop/"
                                        session-unified-dir))

(setq desktop-base-lock-name
      (concat ".emacs.desktop"
              (if (boundp 'server-name)
                  (concat "-" server-name))
              ".lock"))

;; (debug)

(setq desktop-base-file-name
      (concat "emacs-desktop"
              (if (boundp 'server-name)
                  (concat "-" server-name))))

;; Since all lists will be truncated when saved, it is important to
;; have a high default history length, for example. If that is not
;; enough, follow the suggestions in the doc-string of
;; ‘desktop-globals-to-save’:

;;   An element may be variable name (a symbol) or a cons cell of
;;   the form (VAR . MAX-SIZE), which means to truncate VAR’s value
;;   to at most MAX-SIZE elements (if the value is a list) before
;;   saving the value.


;; Auto-Saving the Desktop

;; I’m starting to work on a new package called desktop-recover.el
;; with some improvements like this. Alternatively, you can just add
;; something like this to your init file to auto-save your desktop
;; when Emacs is idle: – Doom


;; (defun my-desktop-save ()
;;   (interactive)
;;   ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
;;   (let ((owner (or (desktop-owner) -1)))
;;     (if (eq owner (emacs-pid))
;;       (desktop-save desktop-dirname)
;;       ;; (desktop-save-in-desktop-dir)
;;       (error "You %d are not the desktop owner %d."
;;              (emacs-pid) owner))))

(defvar *desktop-save-filename* (expand-file-name desktop-base-file-name desktop-dirname))
;; (setq *desktop-save-filename* (expand-file-name desktop-base-file-name desktop-dirname))

;; (when (or (not *emacs-in-init*) (not reloading-libraries))
(when (or *emacs-in-init* reloading-libraries)
  ;setting to nil so it will be asked from user.
  (setq *desktop-save-filename* nil))

;; might be the reason for Terminal 0 is locked.
;; after start check M-: (debug)
;; in case of timeout and no timeout
(defun find-desktop-file (prompt desktop-dir default-file)
  (let ((default-local-file (concat default-file "-local")))
    (if (file-directory-p desktop-dir)
        (let ((default-file-path (expand-file-name default-local-file desktop-dir)))
          (progn
            ;; first try to amke default "-local" file to be available
            (unless (file-exists-p default-file-path)
              (ignore-errors
                (message "desktop file %s do not exists, trying to check it out file %s."
                         default-file-path default-file-path)
                (vc-checkout-file default-local-file)))
            ;; after that provide option to user if he want to chose other file also.
            (let ((file (read-file-name-timeout 20
                                                prompt     ;prompt
                                                desktop-dir ;dir
                                                default-local-file ;default file name
                                                'confirm           ;mustmatch
                                                default-local-file ;initial
                                                #'(lambda (f)        ;predicate  BUG failing this cause bugs
                                                    (message "f: %s" f)
                                                    (string-match (concat "^"
                                                                          (file-truename (expand-file-name default-file desktop-dir))
                                                                          "-")
                                                                  (file-truename f))))))
              (expand-file-name file desktop-dir))))
      (error "desktop directory %s don't exists." desktop-dir))))

;; (find-desktop-file "select desktop: " "~/tmp/" desktop-base-file-name)

;;;###autoload
(defun desktop-get-desktop-save-filename ()
  (interactive)
  (if *desktop-save-filename*
      *desktop-save-filename*
    (setq *desktop-save-filename*
          (find-desktop-file "select desktop: " desktop-dirname desktop-base-file-name))))

(defun switch-desktop-file ())
;; save desktop
;; kill all file buffer
;; change name of desktop file
;; restore desktop file



(defun desktop-vc-remove (&optional desktop-save-filename)
  "Delete desktop file"
  (interactive "Fdesktop file: ")
  (let* ((desktop-save-filename (or desktop-save-filename *desktop-save-filename*))
         (desktop-base-file-name (file-name-nondirectory desktop-save-filename)))
    (when (file-exists-p desktop-save-filename)
      (put-file-in-rcs desktop-save-filename)
      (delete-file desktop-save-filename))))

(defun desktop-vc-owner (&optional desktop-save-filename)
  (interactive "fdesktop file: ")
  (let* ((desktop-save-filename  (or desktop-save-filename
                                     *desktop-save-filename*))
         (retval                 (desktop-owner (dirname-of-file desktop-save-filename))))
    (when (emacs-process-p retval)
      retval)))

(defun desktop-vc-save (&optional desktop-save-filename)
  (interactive "Fdesktop file: ")
  (make-session-unified-dir "desktop")
  (let* ((desktop-save-filename (or desktop-save-filename
                                    *desktop-save-filename*))
         (desktop-base-file-name (file-name-nondirectory desktop-save-filename)))
    (desktop-save (dirname-of-file desktop-save-filename))
    (if (file-exists-p desktop-save-filename)
        (put-file-in-rcs desktop-save-filename))
    (setq desktop-file-modtime (nth 5
                                    (file-attributes desktop-save-filename)))))
;; (desktop-full-file-name)

;; NOTE:
;; (setq desktop-restore-eager 2)
(setq desktop-restore-eager 0) ;; for avoiding error from read only buffer when applying pabber-expand-mode


;;   (and (desktop-owner)
;;        (memq desktop-load-locked-desktop '(nil ask))
;;        (or (null desktop-load-locked-desktop)
;;            (daemonp)
;;            (not (y-or-n-p (format "Warning: desktop file appears to be in use by PID %s.\n\
;; Using it may cause conflicts.  Use it anyway? " owner)))))


(defun desktop-vc-read (&optional desktop-save-filename)
  (interactive "fdesktop file: ")
  (session-unfiy-notify "desktop-restore-eager value is %s" desktop-restore-eager)
  (let* ((desktop-save-filename (or desktop-save-filename *desktop-save-filename*))
         (desktop-base-file-name (file-name-nondirectory desktop-save-filename)))
    (prog1
        (setq *desktop-vc-read-inprogress* t)
      (lotus-disable-session-saving-immediately)

      ;; (unless (desktop-owner)
      ;;   (condition-case nil
      ;;       (desktop-claim-lock)
      ;;     (file-error (message "Couldn't record use of desktop file")
      ;;                 (sit-for 1))))

      (if
          ;; // sharad
          ;; (unless (ignore-errors
          ;;           (save-window-excursion
          ;;             (apply 'desktop-create-buffer desktop-buffer-args)))
          ;;   (message "Desktop lazily opening Failed."))

          ;; ?? how to ignore error generated here

          (desktop-read (dirname-of-file desktop-save-filename))

          (setq *desktop-vc-read-inprogress* nil)
        (message "desktop-vc-read: desktop read failed."))
      (session-unfiy-notify "finished."))))

(defvar desktop-dirname-tmp nil)
;; remove desktop after it's been read
(add-hook 'desktop-after-read-hook
          #'(lambda ()
              ;; desktop-remove clears desktop-dirname
              (setq desktop-dirname-tmp desktop-dirname)
              (desktop-vc-remove)
              (setq desktop-dirname desktop-dirname-tmp)))

(defvar *my-desktop-save-max-error-count* 6 "")
(defvar *my-desktop-save-error-count* 0 "")

;;;###autoload
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (let ((owner (or (desktop-vc-owner)
                   (if (file-exists-p (desktop-full-lock-name desktop-dirname))
                       -1
                     0))))
    (when t ;;condition-case e
      (if (or (eq owner (emacs-pid))
              (eq owner 0)
              ;; TODO: it was mean to be used as non-obtrusive and non-interctive
              (y-or-n-p (format "Your pid %d are not same as the desktop owner pid %d\nOverwrite existing desktop (might be it was not restore properly at startup)? "
                                (emacs-pid) owner)))
          (if *desktop-save-filename*
              (desktop-vc-save *desktop-save-filename*)
            (error "my-desktop-save: *desktop-save-filename* is nil, run M-x desktop-get-desktop-save-filename"))
        ;; (desktop-save-in-desktop-dir)
        (progn
          (lotus-disable-session-saving)
          ;; (remove-hook 'auto-save-hook #'save-all-sessions-auto-save)
          (error "You %d are not the desktop owner %d. removed save-all-sessions-auto-save from auto-save-hook and kill-emacs-hook by calling M-x lotus-disable-session-saving"
                 (emacs-pid) owner))))))

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

(defun save-all-sessions-auto-save-immediately () (save-all-sessions-auto-save t))

(defun lotus-desktop-saved-session ()
  "check file exists."
  (file-exists-p *desktop-save-filename*))

;; use session-save to save the desktop manually
;;;###autoload
(defun lotus-desktop-session-save ()
  "Save an emacs session."
  (interactive)
  (if *session-unified-desktop-enabled*
      (progn
        (if (lotus-desktop-saved-session)
            (if (y-or-n-p "Overwrite existing desktop (might be it was not restore properly at startup)? ")
                (desktop-vc-save *desktop-save-filename*)
              (message "Session not saved."))
          (desktop-vc-save *desktop-save-filename*)))
    (message
     "*session-unified-desktop-enabled*: %s"
     *session-unified-desktop-enabled*)))


(defun desktop-idle-create-buffers ()
  "Create buffers until the user does something, then stop.
 there are no buffers left to create, kill the timer."
  (let ((tags-add-tables nil))
    (let ((repeat 1))
      (while (and repeat desktop-buffer-args-list)
        (unless (ignore-errors
                  (save-window-excursion
                    (desktop-lazy-create-buffer)))
          (message "Desktop lazily opening Failed."))
        (setq repeat (sit-for 0.2))
        (unless desktop-buffer-args-list
          (when desktop-lazy-timer
            (cancel-timer desktop-lazy-timer)
            (setq desktop-lazy-timer nil))
          (message "Lazy desktop load complete")
          (sit-for 3)
          (message nil))))))

(defadvice desktop-idle-create-buffers (after desktop-idle-complete-actions)
  "This advice will finally run lotus-enable-desktop-restore-interrupting-feature-hook
en all buffer were creaed idly."
  (session-unfiy-notify "After desktop-idle-create-buffers (len desktop-buffer-args-list)=%d"
                        (length desktop-buffer-args-list))
  (unless desktop-buffer-args-list
    (session-unfiy-notify "Now removing advice and running lotus-enable-session-saving-immediately")
    (progn
      (ad-disable-advice 'desktop-idle-create-buffers 'after 'desktop-idle-complete-actions)
      (ad-update 'desktop-idle-create-buffers)
      (ad-activate 'desktop-idle-create-buffers))
    (lotus-enable-session-saving-immediately)))

;; use session-restore to restore the desktop manually

(defvar lotus-construct-desktop-filename-regex-function nil)

(defun lotus-construct-desktop-filename-regex-function-default ()
  (concat "^" (getenv "HOME") "/"
          "." (file-name-nondirectory (directory-file-name session-unified-dir))
          "/"  "desktop" "/emacs-desktop-" server-name))

;; (debug)

;;;###autoload
(defun lotus-desktop-session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if *session-unified-desktop-enabled*
      (progn
        ;; ask user about desktop to restore, and use it for session.
        ;; will set *desktop-save-filename*
        (if (desktop-get-desktop-save-filename)
            (let ((desktop-restore-frames nil)
                  (enable-local-eval t)                ;query
                  (enable-recursive-minibuffers t)
                  (flymake-run-in-place nil)
                  (show-error (called-interactively-p 'interactive))
                  (*constructed-name-desktop-save-filename*
                   (if (functionp lotus-construct-desktop-filename-regex-function)
                       (funcall lotus-construct-desktop-filename-regex-function)
                     (lotus-construct-desktop-filename-regex-function-default))))
              (ignore flymake-run-in-place)
              (setq debug-on-error t)
              (session-unfiy-notify "entering lotus-desktop-session-restore")


              (if (not (string-match *constructed-name-desktop-save-filename* *desktop-save-filename*))
                  (progn
                    (session-unfiy-notify "*desktop-save-filename* is not equal to %s but %s"
                                          *constructed-name-desktop-save-filename*
                                          *desktop-save-filename*)
                    (if (y-or-n-p
                         (format "lotus-desktop-session-restore" "*desktop-save-filename* is not equal to %s but %s\nshould continue with it ? "
                                 *constructed-name-desktop-save-filename*
                                 *desktop-save-filename*))
                        (message "continuing..")
                      (error "desktop file %s is not correct" *desktop-save-filename*)))

                (progn
                  (unless (lotus-desktop-saved-session)
                    (session-unfiy-notify "%s not found so trying to checkout it." *desktop-save-filename*)
                    (vc-checkout-file *desktop-save-filename*))

                  (if (lotus-desktop-saved-session)
                      (progn
                        (session-unfiy-notify "if")
                        (when (memq 'P4 vc-handled-backends)            ;remove P4
                          (setq vc-handled-backends (remove 'P4 vc-handled-backends))
                          (add-to-disable-desktop-restore-interrupting-feature-hook
                           #'(lambda ()
                               (when nil
                                 (add-to-list 'vc-handled-backends 'P4)))))
                        (if show-error
                            (if (desktop-vc-read *desktop-save-filename*)
                                (progn
                                  (session-unfiy-notify "desktop loaded successfully :) [show-error=%s]" show-error)
                                  (lotus-enable-session-saving)
                                  (when sessions-unified-elscreen
                                    (session-unfiy-notify "Do you want to set session of frame? [show-error=%s]" show-error)
                                    (when (y-or-n-p-with-timeout (format "[show-error=%s] Do you want to set session of frame? " show-error)
                                                                 10 t)
                                      (let ((*frame-session-restore* t))
                                        (frame-session-restore (selected-frame))))))
                              (progn
                                (session-unfiy-notify "desktop loading failed :( [show-error=%s]" show-error)
                                (run-at-time "1 sec" nil #'(lambda () (insert "lotus-desktop-session-restore")))
                                (execute-extended-command nil)
                                nil))
                          (condition-case e
                              (if (let ((desktop-restore-in-progress t))
                                    (ignore desktop-restore-in-progress)
                                    (desktop-vc-read *desktop-save-filename*))
                                  (progn
                                    (session-unfiy-notify "desktop loaded successfully :) [show-error=%s]" show-error)
                                    (lotus-enable-session-saving))
                                (progn
                                  (session-unfiy-notify "desktop loading failed :( [show-error=%s]" show-error)
                                  nil))
                            ('error
                             (session-unfiy-notify "Error in desktop-read: %s\n not adding save-all-sessions-auto-save to auto-save-hook" e)
                             (session-unfiy-notify "Error in desktop-read: %s try it again by running M-x lotus-desktop-session-restore" e)
                             (run-at-time "1 sec" nil #'(lambda () (insert "lotus-desktop-session-restore")))
                             (condition-case e
                                 (execute-extended-command nil)
                               ('error (message "M-x lotus-desktop-session-restore %s" e))))))
                        t)
                    (when (y-or-n-p
                           (session-unfiy-notify "No desktop found. or you can check out old %s from VCS.\nShould I enable session saving in auto save and run hook, at kill-emacs ?"
                                                 *desktop-save-filename*))
                      ;; as (defadvice desktop-idle-create-buffers) will not get chance to run it.
                      (session-unfiy-notify "As no desktop file or (lotus-desktop-saved-session) is nil so running hook")
                      (lotus-enable-session-saving-immediately)))
                  (when sessions-unified-elscreen
                    (let ((enable-recursive-minibuffers t))
                      (when t ; (y-or-n-p-with-timeout "Do you wato set session of frame? " 7 t) ;t
                        (let ((*frame-session-restore* t))
                          (frame-session-restore (selected-frame) 'only)))))
                  (session-unfiy-notify "leaving lotus-desktop-session-restore"))))

          (session-unfiy-notify "desktop-get-desktop-save-filename failed")))
    (progn
      (lotus-enable-session-saving-immediately)
      (session-unfiy-notify "*session-unified-desktop-enabled* %s"
                            *session-unified-desktop-enabled*)
      t)))

;; ;; ask user whether to restore desktop at start-up
(when nil
  (add-hook ;; 'after-init-hook
   'lotus-enable-startup-interrupting-feature-hook
   #'(lambda ()
       (run-at-time-or-now 7 'lotus-desktop-session-restore))))

;; Then type ‘M-x session-save’, or ‘M-x session-restore’ whenever you want to save or restore a desktop. Restored desktops are deleted from disk.

;;}}


;; ----------------------------------------------------------------------------
;;;###autoload
(defun desktop-read-alternate (&optional dirname)
  "Read and process the desktop file in directory DIRNAME.
ok for a desktop file in DIRNAME, or if DIRNAME is omitted, look in
rectories listed in `desktop-path'.  If a desktop file is found, it
 processed and `desktop-after-read-hook' is run.  If no desktop file
 found, clear the desktop and run `desktop-no-desktop-file-hook'.
is function is a no-op when Emacs is running in batch mode.
 returns t if a desktop file was loaded, nil otherwise."
  (interactive)
  (unless noninteractive
    (setq desktop-dirname
          (file-name-as-directory
           (expand-file-name
            (or
             ;; If DIRNAME is specified, use it.
             (and (< 0 (length dirname)) dirname)
             ;; Otherwise search desktop file in desktop-path.
             (let ((dirs desktop-path))
               (while (and dirs
                           (not (file-exists-p
                                 (desktop-full-file-name (cl-first dirs)))))
                 (setq dirs (cl-rest dirs)))
               (and dirs (cl-first dirs)))
             ;; If not found and `desktop-path' is non-nil, use its first element.
             (and desktop-path (cl-first desktop-path))
             ;; Default: Home directory.
             "~"))))
    (if (file-exists-p (desktop-full-file-name))
        ;; Desktop file found, but is it already in use?
        (let ((desktop-first-buffer nil)
              (desktop-buffer-ok-count 0)
              (desktop-buffer-fail-count 0)
              (owner (desktop-owner))
              ;; Avoid desktop saving during evaluation of desktop buffer.
              (desktop-save nil))
          (if (and owner
                   (memq desktop-load-locked-desktop '(nil ask))
                   (or (null desktop-load-locked-desktop)
                       (not (y-or-n-p
                             (format
                              "Warning: desktop file appears to be in use by PID %s.\nUsing it may cause conflicts.  Use it anyway? "
                              owner)))))
              (let ((default-directory desktop-dirname))
                (setq desktop-dirname nil)
                (run-hooks 'desktop-not-loaded-hook)
                (unless desktop-dirname
                  (session-unfiy-notify "Desktop file in use; not loaded.")))
            (desktop-lazy-abort)
            ;; Evaluate desktop buffer and remember when it was modified.
            (load (desktop-full-file-name) t t t)
            (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
            ;; If it wasn't already, mark it as in-use, to bother other
            ;; desktop instances.
            (if t ;; unless owner
                (condition-case nil
                    (desktop-claim-lock)
                  (file-error (session-unfiy-notify "Couldn't record use of desktop file")
                              (sit-for 1))))

            ;; `desktop-create-buffer' puts buffers at end of the buffer list.
            ;; We want buffers existing prior to evaluating the desktop (and
            ;; not reused) to be placed at the end of the buffer list, so we
            ;; move them here.
            (mapc 'bury-buffer
                  (nreverse (cl-rest (memq desktop-first-buffer (nreverse (buffer-list))))))
            (switch-to-buffer (cl-first (buffer-list)))
            (run-hooks 'desktop-delay-hook)
            (setq desktop-delay-hook nil)
            (run-hooks 'desktop-after-read-hook)
            (session-unfiy-notify "Desktop: %d buffer%s restored%s%s."
                                  desktop-buffer-ok-count
                                  (if (= 1 desktop-buffer-ok-count) "" "s")
                                  (if (< 0 desktop-buffer-fail-count)
                                      (format ", %d failed to restore" desktop-buffer-fail-count)
                                    "")
                                  (if desktop-buffer-args-list
                                      (format ", %d to restore lazily"
                                              (length desktop-buffer-args-list))
                                    ""))
            t))
      ;; No desktop file found.
      (desktop-clear)
      (let ((default-directory desktop-dirname))
        (run-hooks 'desktop-no-desktop-file-hook))
      (session-unfiy-notify "desktop-read-alternate: No desktop file.")
      nil)))


(defmacro desktop-get-readonly-proof-mode (modefn)
  `#'(lambda (desktop-buffer-locals)
       (unless (or desktop-buffer-read-only buffer-read-only)
         (condition-case e
             (,modefn 1)
           ('error (session-unfiy-notify "%s: %s" ,modefn e))))))

;;; desktop-unified.el ends here
