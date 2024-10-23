;;; basic-utils.el --- Basic utilities               -*- lexical-binding: t; -*-

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

(provide 'basic-utils)


(require 'pp)


(defvar *emacs-in-init* nil)
(defvar exclude-lib nil)

;;;###autoload
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

;;;###autoload
(defvar reloading-libraries nil "used in session-conf.el")


;;;###autoload
(defun package-dir-add-to-loadpath (package-dir &optional recursive)
  (when (file-directory-p package-dir)
    (mapc (if recursive
              #'(lambda (path)
                  (add-to-list 'load-path path)
                  (let ((default-directory path))
                    (ignore default-directory)
                    (normal-top-level-add-subdirs-to-load-path)))
            #'(lambda (path)
                (add-to-list 'load-path path)))
          (cl-remove-if-not #'file-directory-p
                            (directory-files package-dir t "[a-zA-Z]+")))))

;;;###autoload
(defun global-set-key-if-unbind (key cmd)
  "Set binding for key if there is no  existing binding for key."
  ;; (interactive)
  (let ((bindedcmd (key-binding key t)))
    (if bindedcmd
        (when (or (not *emacs-in-init*) (not reloading-libraries))
          (message "key %s already have binded with command %s, can't bind to %s."
                   key bindedcmd cmd))
      (global-set-key key cmd))))

;;;###autoload
(defun global-set-key-warn-if-bind (key cmd)
  "Set binding for key if there is no  existing binding for key."
  ;; (interactive)
  (let ((bindedcmd (key-binding key t)))
    (if bindedcmd
        (when (or (not *emacs-in-init*) (not reloading-libraries))
          (message "key %s already have binded with command %s, but binding to %s."
                   key bindedcmd cmd)))
    (global-set-key key cmd)))

;;;###autoload
(defun keymap-set-key-if-unbind (map key cmd)
  "Set binding for key if there is no  existing binding for key."
  ;; (interactive)
  (let ((bindedcmd (key-binding key t)))
    (if bindedcmd
        (when (or (not *emacs-in-init*) (not reloading-libraries))
          (message "key %s already have binded with command %s, can't bind to %s."
                   key bindedcmd cmd))
      (define-key map key cmd))))


;;;###autoload
(defun global-unset-key-if-bound (key cmd)
  "Set binding for key if there is no  existing binding for key."
  ;; (interactive)
  (let ((bindedcmd (key-binding key t)))
    (if (equal bindedcmd cmd)
        (global-unset-key key)
      (message "key %s is not bounded with command %s, can't unset to %s."
               key bindedcmd cmd))))

;;;###autoload
(defun global-unset-key-warn-if-bound (key cmd)
  "Set binding for key if there is no  existing binding for key."
  ;; (interactive)
  (let ((bindedcmd (key-binding key t)))
    (if (equal bindedcmd cmd)
        (global-unset-key key)
      (message "key %s is not bounded with command %s, can't unset to %s."
               key bindedcmd cmd))))

;;;###autoload
(defun keymap-unset-key-if-bound (map key cmd)
  "Set binding for key if there is no  existing binding for key."
  ;; (interactive)
  (let ((bindedcmd (key-binding key t)))
    (if (equal bindedcmd cmd)
        (define-key map key cmd)
      (message "key %s is not bounded with command %s, can't unset to %s."
               key bindedcmd cmd))))

;;;###autoload
(defun fprint (dir)
  "Print the current buffer with same file name."
  (interactive "DDirqectory to put: ")
  (let* ((fname (file-name-nondirectory
                 (buffer-file-name)))
         (pname (concat (or dir default-directory) fname ".ps")))
    (ps-print-in-file pname)))



;;{{{ define xrequire

;;;###autoload
(defun xrequire (feature)
  (unless (member feature exclude-lib)
    (if (not running-xemacs)
        (require feature nil t)
      (require feature nil))))

;;;###autoload
(defun irequire (feature)
  (ignore-errors
    (unless (member feature exclude-lib)
      (if (not running-xemacs)
          (require feature nil t)
        (require feature nil)))))


;;}}}

;; Are we running XEmacs or Emacs?
;;;###autoload
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))


;;;###autoload
(defun afind-if (fun list) ;; anaphoric
  (let ((result
         (funcall fun (cl-first list))))
    (if result
        result
      (if list (afind-if fun (cl-rest list))))))

(eval-when-compile
  '(when (featurep 'notify)
     (require 'notify)))

;;;###autoload
(defun lotus-may-stringfy (obj)
  (cond
   ((numberp obj) obj)
   (t (prin1-to-string obj))))

;;;###autoload
(defun lotus-message-notify (title fmt &rest args)
  (unless (stringp title)
    (error "lotus-message-notify title %s argument is not string." title))
  (unless (stringp fmt)
    (error "lotus-message-notify fmt %s argument is not string." fmt))
  (message "%s: %s"
           title
           (apply 'format fmt
                  (mapcar #'lotus-may-stringfy args)))
  (when (fboundp 'notify)
    (notify title
            (apply 'format fmt
                   (mapcar #'lotus-may-stringfy args)))))

;;;;;###autoload
;; (defun lotus-message-notify (title fmt &rest args)
;;   (unless (stringp title)
;;     (error "lotus-message-notify title %s argument is not string." title))
;;   (unless (stringp fmt)
;;     (error "lotus-message-notify fmt %s argument is not string." fmt))
;;   (message "%s: %s"
;;            title
;;            args)
;;   (when (fboundp 'notify)
;;     (notify title
;;             args)))

;;;###autoload
(defun add-to-hook (hook fn &optional append local)
  (interactive)
  (add-hook hook
            fn
            append
            local)
  (ignore-errors
    (lotus-message-notify "add-to-hook" "in hook %s adding\n\t function:\n%s"
                          hook
                          (pp-to-string fn)
                          hook)))

;;;###autoload
(defun run-each-hooks (hook)
  (dolist (f (symbol-value hook))
    (condition-case e
        (progn
          (lotus-message-notify "run-each-hooks" "%s: running function:\n%s" hook (pp-to-string f))
          (funcall f))
      (error
       (lotus-message-notify "run-each-hooks" "Error: function:\n%s error %s" (pp-to-string f) e)))))

;;;###autoload
(defun run-each-debug-hooks (hook)
  (dolist (f (symbol-value hook))
    (condition-case e
        (progn
          (lotus-message-notify "run-each-hooks" "%s: running function:\n%s" hook (pp-to-string f))
          (funcall f))
      (error
       (lotus-message-notify "run-each-hooks" "Error: function:\n%s error %s" (pp-to-string f) e)))))

;;;###autoload
(defvar *undefine-function-alist* nil "undefine-function-alist")

;;;###autoload
(defun undefine-function-remember (fnsym)
  "Use (redefine-function-remembered fnsym) to redefine."
  (unless (eq fnsym 'ignore)
    (push (cons fnsym (symbol-function fnsym))
          *undefine-function-alist*)
    (defalias fnsym 'ignore)))

;;;###autoload
(defun redefine-function-remembered (fnsym)
  "Use (undefine-function-remember fnsym) to undefine."
  (let ((fdef (assoc fnsym *undefine-function-alist*)))
    (if fdef
        (progn
          (fset fnsym (cl-rest fdef))
          (setq *undefine-function-alist*
                (del-alist (cl-first fdef) *undefine-function-alist*)))
      (message "def for %s function is not available." fnsym))))

;;;###autoload
(defun load-dir-files (dir)
  (let (load-file-with-errors)
    (when (file-directory-p dir)
      (byte-recompile-directory dir 0)
      (mapc #'(lambda (f)
                (if (not (ignore-errors (load-file f)))
                    (push f load-file-with-errors)))
            (directory-files dir t "^[a-zA-Z0-9-]+\.elc$"))
      (if load-file-with-errors
          (mapc #'load-file
                load-file-with-errors)
        t))))

;;;###autoload
(defun require-dir-libs (dir)
  (let (load-lib-with-errors
        reloading-libraries)
    (when (file-directory-p dir)
      (ignore-errors (byte-recompile-directory dir 0))
      (mapc #'(lambda (lib)
                (let ((feature (if (string-match "\\(.\+\\)\.el" lib)
                                   (intern (match-string 1 lib)))))
                  (if feature
                      (unless (and (message "now loading %s.el" feature)
                                   (with-report-error "check"
                                       (require feature)))
                        (push feature load-lib-with-errors)))))
            (directory-files dir nil "^[a-zA-Z0-9-]+\.el$"))
      (if load-lib-with-errors
          (progn
            (setq reloading-libraries t)
            (message "now loading files ( %s ) with errors." load-lib-with-errors)
            (mapc #'(lambda (f)
                      (message "now loading file with error %s.el" f)
                      (with-report-error "check"
                          (require f)))
                  load-lib-with-errors))
        (message "all library loaded in %s directory without error." dir))
      t)))

;;;###autoload
(defun autoload-dir-libs (dir)
  (let (load-lib-with-errors
        reloading-libraries)
    (when (file-directory-p dir)
      (ignore-errors (byte-recompile-directory dir 0))
      (mapc #'(lambda (lib)
                (let ((feature (if (string-match "\\(.\+\\)\.el" lib)
                                   (intern (match-string 1 lib)))))
                  (if feature
                      (unless
                          (and (message "now loading %s.el" feature)
                               (with-report-error "check"
                                   (load-lib-autoloads feature)))

                        (push feature load-lib-with-errors)))))
            (directory-files dir nil "^[a-zA-Z0-9-]+\.el$"))
      (if load-lib-with-errors
          (progn
            (setq reloading-libraries t)
            (message "now loading files ( %s ) with errors." load-lib-with-errors)
            (mapc #'(lambda (f)
                      (message "now loading file with error %s.el" f)
                      (with-report-error "check"
                          (load-lib-autoloads f)))
                  load-lib-with-errors))
        (message "all library loaded in %s directory without error." dir))
      t)))

;;;###autoload
(defun add-element-to-lists (element lists)
  (dolist (list lists)
    (add-hook (intern (concat (symbol-name list) "-mode-hook")) element)))

;;;###autoload
(defun remove-element-from-lists (element lists)
  (dolist (list lists)
    (remove-hook (intern (concat (symbol-name list) "-mode-hook")) element)))

(defvar pgm-langs '(java
                    c
                    c++
                    perl
                    lisp
                    emacs-lisp
                    cperl
                    js
                    espresso
                    ruby
                    sh
                    python) "Langauge modes.")

(defvar text-langs '(muse
                     text))

(defvar reader-requester '(rfcview) "Modes that need reader.")

(defvar mode-used '(org planner)  "Modes used.")

(setq mode-used (append mode-used pgm-langs))


;;{{ Pathname Utilities
;;;###autoload
(defun  pathname-end-with-/ (path)
  "Check if path name end with /"
  (equal (elt path (- (length path) 1)) ?/))

;;;###autoload
(defun pathname-delete-trailing-/ (path)
  (if (pathname-end-with-/ path)
      (pathname-delete-trailing-/ (subseq path 0 (- (length path) 2)))
    path))

;;;###autoload
(defun pathname-equal (p1 p2)
  "Pathname equality"
  (apply #'string-equal
         (mapcar #'pathname-delete-trailing-/ (list p1 p2))))




;;{{ --debug-init
(message "debug-on-error %s" debug-on-error)
;;}}


;;{{

;;;###autoload
(defun run-at-time-or-now (time fn)
  "Run FN at TIME if numeric is otherwise run now only."
  ;; (lotus-message-notify "run-at-time-or-now" "will run %s after %d sec" fn time)
  (if (numberp time)
      (run-with-timer time nil fn)
    (funcall fn)))

;;;###autoload
(defun run-at-time-or-now-arg (time fn arg)
  "Run FN with ARG at TIME if numeric is otherwise run now only."
  (if (numberp time)
      (run-with-timer time nil
                      #'(lambda (a) (funcall (cl-first a) (cl-rest a)))
                      (cons fn arg))
    (funcall fn arg)))

;;;###autoload
(defun my-delete-timer ()
  (interactive)
  (dolist (timer timer-list)
    (let ()
      (when (yes-or-no-p (format "Remove timer: %s" timer))
        (message "removing timer %s" timer)
        (delete timer timer-list)
        (message "removed timer %s list is now %s" timer timer-list)))))

;;; basic-utils.el ends here
