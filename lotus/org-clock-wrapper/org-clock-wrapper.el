;;; org-clock-wrapper.el --- org-context-clock-api               -*- lexical-binding: t; -*-

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

(provide 'org-clock-wrapper)


(require 'time-stamp)
(eval-when-compile
  (require 'helm-source))
(require 'helm-core)
(require 'helm-source)
(require 'org)
(require 'org-macs)
(require 'org-duration)
(require 'org-timer)
(require 'org-clock)

(require 'basic-utils)
(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))
(eval-when-compile
  (require 'org-macs))


;; "correction org-timer.el"
(defun replace-org-timer-set-timer (&optional opt)
    "Prompt for a duration in minutes or hh:mm:ss and set a timer.

If `org-timer-default-timer' is not \"0\", suggest this value as
the default duration for the timer.  If a timer is already set,
prompt the user if she wants to replace it.

Called with a numeric prefix argument, use this numeric value as
the duration of the timer in minutes.

Called with a `C-u' prefix arguments, use `org-timer-default-timer'
without prompting the user for a duration.

With two `C-u' prefix arguments, use `org-timer-default-timer'
without prompting the user for a duration and automatically
replace any running timer.

By default, the timer duration will be set to the number of
minutes in the Effort property, if any.  You can ignore this byh
using three `C-u' prefix arguments."
    (interactive "P")
    (when (and org-timer-start-time
               (not org-timer-countdown-timer))
      (user-error "Relative timer is running.  Stop first"))
    (let* ((default-timer  (if (numberp org-timer-default-timer) ;; `org-timer-default-timer' used to be a number, don't choke:
                               (number-to-string org-timer-default-timer)
                             org-timer-default-timer))
           (clocked-time   (org-clock-get-clocked-time))
           (effort-minutes (or (ignore-errors (org-get-at-eol 'effort-minutes 1))
                               (if (org-entry-get nil "Effort")
                                   (org-duration-to-minutes (org-entry-get nil "Effort")))))
           (remianing-effort-minutes (if (and effort-minutes
                                              clocked-time
                                              (>= effort-minutes clocked-time))
                                         (- effort-minutes clocked-time)
                                         effort-minutes))
           (minutes (or (and (not (equal opt '(64)))
                             effort-minutes
                             (number-to-string remianing-effort-minutes))
                        (and (numberp opt)
                             (number-to-string opt))
                        (and (consp opt)
                             default-timer)
                        (and (stringp opt)
                             opt)
                        (read-from-minibuffer "How much time left? (minutes or h:mm:ss) "
                                              (and (not (string-equal default-timer "0"))
                                                   default-timer)))))
      (message "effort-minutes %s clocked-time %s remianing-effort-minutes %s"
               effort-minutes clocked-time remianing-effort-minutes)
      (when (string-match "\\`[0-9]+\\'" minutes)
        (let* ((mins (string-to-number minutes))
               (h (/ mins 60))
               (m (% mins 60)))
          (setq minutes (format "%02d:%02d" h m)))
        (setq minutes (concat minutes ":00")))
      (if (not (string-match "[0-9]+" minutes))
          (org-timer-show-remaining-time)
          (let ((secs (org-timer-hms-to-secs (org-timer-fix-incomplete minutes)))
                (hl (org-timer--get-timer-title)))
            (ignore hl)
            (if (or (not org-timer-countdown-timer)
                    (equal opt '(16))
                    (y-or-n-p "Replace current timer? "))
                (progn
                  (when (timerp org-timer-countdown-timer)
                    (cancel-timer org-timer-countdown-timer))
                  (setq org-timer-countdown-timer-title
                        (org-timer--get-timer-title))
                  (setq org-timer-countdown-timer
                        (org-timer--run-countdown-timer
                         secs org-timer-countdown-timer-title))
                  (run-hooks 'org-timer-set-hook)
                  (setq org-timer-start-time
                        (time-add (current-time) (seconds-to-time secs)))
                  (setq org-timer-pause-time nil)
                  (org-timer-set-mode-line 'on))
                (message "No timer set"))))))

;; (advice-add 'org-timer-set-timer :around #'replace-org-timer-set-timer)

;; (add-function :override (symbol-function 'org-timer-set-timer) #'replace-org-timer-set-timer)


(defun lotus-org-marker-selection-line (marker)
  "Insert a line for the clock selection menu.
  And return a cons cell with the selection character integer and the marker
  pointing to it."
  (when (marker-buffer marker)
    (with-current-buffer (org-base-buffer (marker-buffer marker))
      (org-with-wide-buffer
       (progn ;; ignore-errors
         (goto-char marker)
         (let* ((cat (org-get-category))
                (heading (org-get-heading 'notags))
                (prefix (save-excursion
                          (org-back-to-heading t)
                          (looking-at org-outline-regexp)
                          (match-string 0)))
                (task (substring
                       (org-fontify-like-in-org-mode
                        (concat prefix heading)
                        org-odd-levels-only)
                       (length prefix))))
           (ignore cat)
           (when task ;; (and cat task)
             ;; (insert (format "[%c] %-12s  %s\n" i cat task))
             ;; marker
             (cons task marker))))))))

(defvar *org-clock-select-task-postpone-timer* nil)

;;;###autoload
(defun replace-org-clock-select-task (&optional prompt)
  (lwarn 'org-clock-select-task :debug "%s: begin replace-org-clock-select-task" (time-stamp-string))
  (prog1
      (lotus-with-other-frame-event-debug "replace-org-clock-select-task" :restart
        (lwarn 'org-clock-wrapper :debug "replace-org-clock-select-task: lotus-with-other-frame-event-debug")
        (condition-case nil
            (let ((helm-sources nil))
             (when (marker-buffer org-clock-default-task)
               (push (helm-build-sync-source "Default Task"
                       :candidates (list (lotus-org-marker-selection-line org-clock-default-task))
                       :action (list ;; (cons "Select" 'identity)
                                (cons "Clock in and track" #'identity)))
                     helm-sources))

             (when (marker-buffer org-clock-interrupted-task)
               (push (helm-build-sync-source "The task interrupted by starting the last one"
                       :candidates (list (lotus-org-marker-selection-line org-clock-interrupted-task))
                       :action     (list (cons "Clock in and track" #'identity)))
                     helm-sources))

             (when (and (org-clocking-p)
                        (marker-buffer org-clock-marker))
               (push (helm-build-sync-source "Current Clocking Task"
                       :candidates (list (lotus-org-marker-selection-line org-clock-marker))
                       :action     (list (cons "Clock in and track" #'identity)))
                     helm-sources))

             (when org-clock-history
               (push (helm-build-sync-source "Recent Tasks"
                       :candidates (mapcar #'lotus-org-marker-selection-line org-clock-history)
                       :action     (list (cons "Clock in and track" #'identity)))
                     helm-sources))
             (lotus-with-no-active-minibuffer-if
                 (let ((postpone-secs 10))
                   (message "replace-org-clock-select-task: [minibuffer] lotus-with-no-active-minibuffer-if launching after %d seconds" postpone-secs)
                   (unless *org-clock-select-task-postpone-timer*
                     (setq *org-clock-select-task-postpone-timer*
                           (run-at-time-or-now postpone-secs #'(lambda ()
                                                                 (setq *org-clock-select-task-postpone-timer* nil)
                                                                 ;; org-clock-select-task call from org-clock-in
                                                                 (replace-org-clock-select-task prompt))))))
               (helm :sources helm-sources
                     :buffer "*helm-org-clock-select-task*")))
          ((quit error) (message "Exit"))))
    (lwarn 'org-clock-select-task :debug "%s: finisha replace-org-clock-select-task" (time-stamp-string))))

;;;###autoload
(defalias 'override--org-clock-select-task #'replace-org-clock-select-task)

;;;###autoload
(defun org-clock-wrapper-insinuate ()
  (interactive)
  (with-eval-after-load "org-clock"
    (add-function :override (symbol-function 'org-clock-select-task)
                  #'override--org-clock-select-task)
    (message "calling org-clock-wrapper-insinuate")))

;;;###autoload
(defun org-clock-wrapper-uninsinuate ()
  (interactive)
  (remove-function (symbol-function 'org-clock-select-task)
                   #'override--org-clock-select-task)
  (message "calling org-clock-wrapper-uninsinuate"))


;; (replace-org-clock-select-task org-clock-history)
;; (org-clock-select-task org-clock-history)

(when nil ;;testing
  (progn
    (defun sacha/helm-select-clock (clocks)
      (org-context-clock-debug :debug "sacha marker %s" (cl-first clocks))
      (helm
       (list
        (helm-build-sync-source "Select matching clock"
          :candidates (mapcar 'lotus-org-marker-selection-line clocks)
          :action (list ;; (cons "Select" 'identity)
                   (cons "Clock in and track" #'identity))
          :history 'org-refile-history)))))
        ;; (helm-build-dummy-source "Create task"
        ;;   :action (helm-make-actions
        ;;            "Create task"
        ;;            'sacha/helm-org-create-task))

  (progn
    (helm
     (list
      (helm-build-sync-source "Select matching clock"
        :candidates (mapcar 'lotus-org-marker-selection-line clocks)
        :action (list ;; (cons "Select" 'identity)
                 (cons "Clock in and track" #'identity))
        :history 'org-refile-history))))

      ;; (helm-build-dummy-source "Create task"
      ;;   :action (helm-make-actions
      ;;            "Create task"
      ;;            'sacha/helm-org-create-task))


  (progn
    ;; http://kitchingroup.cheme.cmu.edu/blog/2015/01/30/More-adventures-in-helm-more-than-one-action/
    (setq data '(("John" . "john@email.com")
                 ("Jim" . "jim@email.com")
                 ("Jane" . "jane@email.com")
                 ("Jill" . "jill@email.com")))


    (defun open-email (candidates)
      "Compose an email to the candidates. Fill in the addresses and
move point to the subject."
      (compose-mail)
      (message-goto-to)
      (insert
       (mapconcat
        'identity
        (helm-marked-candidates)
        ","))
      (message-goto-subject))

    (setq some-helm-source
          `((name . "HELM at the Emacs")
            (candidates . ,data)
            (action . (("show email address" . (lambda (candidate)
                                                 (message-box
                                                  "selected: %s"
                                                  (helm-marked-candidates))))
                       ("send email" . open-email)))))

    (setq some-other-helm-source
          `((name . "HELM at the Test")
            (candidates . ,data)
            (action . (("show email address" . (lambda (candidate)
                                                 (message-box
                                                  "selected: %s"
                                                  (helm-marked-candidates))))
                       ("send email" . open-email)))))

    (helm :sources '(some-helm-source some-other-helm-source))))

;;; org-clock-wrapper.el ends here
