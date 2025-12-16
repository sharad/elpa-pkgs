;;; org-clock-in-if-not.el --- org-context-clock-api               -*- lexical-binding: t; -*-

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

(provide 'org-clock-in-if-not)


(require 'basic-utils)
(require 'startup-hooks)
(eval-when-compile
  (require 'lotus-misc-utils))
(require 'lotus-misc-utils)
(require 'lotus-idle-utils)
(require 'lotus-utils-debug)
(eval-when-compile
  (require 'org-misc-utils-lotus))
(require 'org-misc-utils-lotus)
(require 'org)
(require 'org-clock)
(require 'org-macs)
(require 'startup-hooks)
(require 'time-stamp)


(defvar org-ci-if-not-debug nil "Debug org-ci-if-not")
(defvar org-ci-if-not-debug-uncond nil "org-ci-if-not-debug-uncond")

(defvar org-clock-in-if-not-at-time-timer nil)


;;;###autoload
(defun org-ci-if-not-enable-debug ()
  (interactive)
  (setq org-ci-if-not-debug t))
;;;###autoload
(defun org-ci-if-not-disable-debug ()
  (interactive)
  (setq org-ci-if-not-debug nil))


(defun org-ci-if-not-debug (level &rest args)
  (when org-ci-if-not-debug
    (when (cl-first args)
      (let* ((fmt (cl-first args))
             (fmt (format "%s: %s" (time-stamp-string) fmt))
             (args (cons fmt (cl-rest args))))
        (apply #'format args)
        (when (member level '(:emergency :error :warning :debug))
          ;; (apply #'lwarn 'org-ci-if-not level args)
          (apply #'lwarn 'org-ci-if-not level args))
        (unless (eq level :nodisplay)
          (apply #'message args))))))

(defun org-ci-if-not-message (&rest args)
  (apply #'message args)
  (apply #'org-ci-if-not-debug :debug args))

(defun org-ci-if-not-debug-uncond (&rest args)
  (when org-ci-if-not-debug-uncond
    (apply #'org-ci-if-not-message args)))


;; org-refile-targets is set in org-misc-utils-lotus package
;;;###autoload
(defun org-clock-in-refile (refile-targets)
  ;; using plain competing-read not helm
  ;; for helm use below
  ;; https://sachachua.com/blog/2015/03/getting-helm-org-refile-clock-create-tasks/
  ;; done in safe-org-refile-get-location in org-misc-utils-lotus
  (org-with-refile file loc (or refile-targets org-refile-targets) "Refile clock-in"
    (let ((buffer-read-only nil))
      (org-clock-in))))
;; testing
;; (org-clock-in-refile nil)
(defvar org-clock-in-if-not-delay 100 "org-clock-in-if-not-delay")
(defvar org-donot-try-to-clock-in nil
  "Not try to clock-in, require for properly creating frame
especially for frame-launcher function.")

;;;###autoload
(defun org-clock-in-if-not ()
  (interactive)
  (org-ci-if-not-debug :debug "org-clock-in-if-not: begin")
  (lotus-run-unobtrusively                    ;heavy task
    (unless (or org-donot-try-to-clock-in
                (org-clock-is-active))
      ;; (org-clock-goto t)
      (org-ci-if-not-debug :debug "org-clock-in-if-not: really calling")
      (lotus-with-no-active-minibuffer-if
          (let ((postpone-secs 10))
            (org-ci-if-not-debug :debug "org-clock-in-if-not: [minibuffer] lotus-with-no-active-minibuffer-if launching after %d seconds" postpone-secs)
            (unless org-clock-in-if-not-at-time-timer
              (org-clock-in-if-not-at-time postpone-secs)))
        (lotus-with-other-frame-event-debug "org-clock-in-if-not" :restart
          (org-ci-if-not-debug :debug "org-clock-in-if-not: [body] lotus-with-no-active-minibuffer-if")
          ;; (message "Enable Disabel with org-clock-in-if-not-enable org-clock-in-if-not-disable")
          (condition-case nil
              (if org-clock-history
                  (let (buffer-read-only)
                    (org-clock-in '(4)))
                ;; with-current-buffer should be some real file
                (org-clock-in-refile nil))
            ((quit error) (message "Enable/Disable with org-clock-in-if-not-enable/org-clock-in-if-not-disable")))
          (org-ci-if-not-debug :debug "org-clock-in-if-not: finished"))))))
;;;###autoload
(defun org-clock-in-if-not-disable ()
  (interactive)
  (setq org-donot-try-to-clock-in t))
;;;###autoload
(defun org-clock-in-if-not-enable ()
  (interactive)
  (setq org-donot-try-to-clock-in nil))


;;;###autoload
(defun org-clock-in-if-not-at-time (delay)
  (prog1
      (setq org-clock-in-if-not-at-time-timer
            (run-at-time-or-now delay
                                #'(lambda ()
                                    (if (any-frame-opened-p)
                                        (org-clock-in-if-not)))))
    (message "%s: org-clock-in-if-not-at-time: begin timer=%s after %d secs"
             (time-stamp-string)
             org-clock-in-if-not-at-time-timer
             delay)))

;;;###autoload
(defun org-clock-in-if-not-at-time-delay ()
  (org-ci-if-not-debug :debug "org-clock-in-if-not-at-time-delay: begin after %d secs"
                       org-clock-in-if-not-delay)
  (org-clock-in-if-not-at-time org-clock-in-if-not-delay))

;;;###autoload
(defun org-clock-in-if-not-at-time-delay-fn ()
  (message "%s: org-clock-in-if-not-at-time-delay-fn begin"
           (time-stamp-string))
  (org-clock-in-if-not-at-time-delay))


(defun org-clock-out-if-active ()
  (if (and (org-clock-is-active)
           (last-frame-opened-p)
           (y-or-n-p-with-timeout (format "Do you want to clock out current task %s: " org-clock-heading)
                                  7
                                  nil))
      (org-with-clock-writeable
        (let (org-log-note-clock-out)
          (if (org-clock-is-active)
              (org-clock-out))))))

;;; org-clock-in-if-not.el ends here
