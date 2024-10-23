;;; org-clock-utils-lotus.el --- copy config         -*- lexical-binding: t; -*-

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

;;; Code:

(provide 'org-clock-utils-lotus)


;; (require 'org-clock-check)
;; (require 'org-clock-hooks)
;; (require 'org-clock-experimental)
;; (require 'org-clock-unnamed-task)
;; (require 'org-clock-wrapper)

(require 'org)
(require 'org-timer)
(require 'org-clock)
(require 'timer-utils-lotus)
;; (require 'startup-hooks)
(eval-when-compile
  '(require 'org-misc-utils-lotus))
(require 'org-misc-utils-lotus)

(defvar lotus-straight-org-clock-persist nil "Control org-clock-persist at time of lotus-straight clock-in")
(defvar lotus-straight-org-clock-auto-clock-resolution nil "Control occ-org-clock-auto-clock-resolution at time of lotus-straight clock-in")

;;;###autoload
(defun lotus-org-clock-load-only ()
  "Load clock-related data from disk, maybe resuming a stored clock."
  (when (and org-clock-persist (not org-clock-loaded))
    (progn
      (if (not (file-readable-p org-clock-persist-file))
          (message "Not restoring clock data; %S not found" org-clock-persist-file)
        (message "Restoring clock data")
        ;; Load history.
        (load-file org-clock-persist-file)
        (setq org-clock-loaded t)
        (pcase-dolist (`(,(and file (pred file-exists-p)) . ,position)
                       org-clock-stored-history)
          (org-clock-history-push position (find-file-noselect file)))))
    (setq org-clock-loaded nil)))

;;;###autoload
(defun lotus-org-clock-resume ()
  (when (and org-clock-persist
             ;; may not work as file is already loaded.
             (not org-clock-loaded))
    (if (not (file-readable-p org-clock-persist-file))
        (message "Not restoring clock data; %S not found" org-clock-persist-file)
      ;; Resume clock.
      (pcase org-clock-stored-resume-clock
        (`(,(and file (pred file-exists-p)) . ,position)
         (with-current-buffer (find-file-noselect file)
           (when (or (not org-clock-persist-query-resume)
                     (y-or-n-p (format "Resume clock (%s) "
                                       (save-excursion
                                         (goto-char position)
                                         (org-get-heading t t)))))
             (goto-char position)
             (let ((org-clock-in-resume 'auto-restart)
                   (org-clock-auto-clock-resolution nil))
               (org-clock-in)
               (org-lotus-modification-post-action)
               (when (org-invisible-p) (org-show-context))))))
        (_ nil)))))

;;;###autoload
(defun lotus-straight-org-clock-clock-in (clock &optional resume start-time)
  "lotus-straight-org-clock-clock-in"
  (progn
    (lotus-org-clock-load-only)
    (prog1
        (let ((org-clock-persist               lotus-straight-org-clock-persist)
              (org-clock-auto-clock-resolution lotus-straight-org-clock-auto-clock-resolution))
          (org-clock-clock-in clock resume start-time))
      (setq org-clock-loaded t))))

;;;###autoload
(defun lotus-straight-org-clock-clock-out (clock &optional fail-quietly at-time)
  "lotus-straight-org-clock-clock-out"
  (let ((org-clock-persist lotus-straight-org-clock-persist)
        (org-clock-auto-clock-resolution lotus-straight-org-clock-auto-clock-resolution))
    (org-clock-clock-out clock fail-quietly at-time)))

(defmacro org-without-org-clock-persist (&rest body)
  "org-without-org-clock-persist"
  `(let ((org-clock-persist lotus-org-unnamed-task-org-clock-persist))
     (lotus-org-clock-load-only)
     (progn ,@body)))
(put 'org-without-org-clock-persist 'lisp-indent-function 0)

(defmacro org-without-org-clock-auto-clock-resolution (&rest body)
  "org-without-org-clock-auto-clock-resolution"
  `(let ((org-clock-auto-clock-resolution lotus-org-unnamed-task-org-clock-auto-clock-resolution))
     ,@body))
(put 'org-without-org-clock-auto-clock-resolution 'lisp-indent-function 0)

(defmacro org-without-org-clock-persist-and-auto-clock-resolution (&rest body)
  "org-without-org-clock-persist"
  `(org-without-org-clock-persist
     (org-without-org-clock-auto-clock-resolution
       ,@body)))
(put 'org-without-org-clock-persist-and-auto-clock-resolution 'lisp-indent-function 0)


(defmacro org-with-clock-position (clock &rest forms)
  "Evaluate FORMS with CLOCK as the current active clock."
  `(with-current-buffer (marker-buffer (cl-first ,clock))
     (save-excursion
       (save-restriction
         (widen)
         (goto-char (cl-first ,clock))
         (beginning-of-line)
         (let (buffer-read-only)
           ,@forms)))))

;; (defun org-clock-get-nth-half-clock-time (marker n)
;;   (let ((org-clock-re
;;          (concat org-clock-string " \\(\\[.*?\\]\\)$")))
;;     (org-with-narrow-to-marker marker
;;       (goto-char (point-min))
;;       (when (re-search-forward org-clock-re nil t n)
;;         (cons (copy-marker (match-end 1) t)
;;               (org-time-string-to-time (match-string 1)))))))


;; (defun org-clock-get-nth-clock-times (marker n)
;;   (let ((org-clock-re
;;          (concat "^[ \t]*" org-clock-string
;;                  " \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
;;                  " *\\sw+.? +[012][0-9]:[0-5][0-9]\\)\\][ \t]*$")))
;;     (org-with-narrow-to-marker marker
;;       (goto-char (point-min))
;;       (when (re-search-forward org-clock-re nil t n)
;;         (list (copy-marker (match-end 1) t)
;;               (org-time-string-to-time (match-string 1))
;;               (org-time-string-to-time (match-string 2)))))))


;;;###autoload
(defun org-clock-get-nth-full-clock-data (marker &optional n)
  (let ((n (or n 1))
        (org-clock-re
         (concat "^[ \t]*" org-clock-string
                 " \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
                 " *\\sw+.? +[012][0-9]:[0-5][0-9]\\)\\][ \t]*$")))
    (org-with-narrow-to-marker marker
      (goto-char (point-min))
      (when (re-search-forward org-clock-re nil t n)
        (list (copy-marker (match-beginning 0) t)
              (copy-marker (match-end 0) t)
              (org-time-string-to-time (match-string 1))
              (org-time-string-to-time (match-string 2)))))))

;;;###autoload
(defun org-clock-get-nth-half-clock-data (marker &optional n)
  (let ((n (or n 1))
        (org-clock-re
         (concat "^[ \t]*" org-clock-string " \\(\\[.*?\\]\\)$")))
    (org-with-narrow-to-marker marker
      (goto-char (point-min))
      (when (re-search-forward org-clock-re nil t n)
        (list
         (copy-marker (match-beginning 0) t)
         (copy-marker (match-end 1) t)
         (org-time-string-to-time (match-string 1)))))))

;;;###autoload
(defun org-clock-get-nth-full-clock-beginning (marker &optional n)
  (nth 0 (org-clock-get-nth-full-clock-data marker n)))
;;;###autoload
(defun org-clock-get-nth-full-clock-end (marker &optional n)
  (nth 1 (org-clock-get-nth-full-clock-data marker n)))
;;;###autoload
(defun org-clock-get-nth-full-clock-start-time (marker &optional n)
  (nth 2 (org-clock-get-nth-full-clock-data marker n)))
;;;###autoload
(defun org-clock-get-nth-full-clock-end-time (marker &optional n)
  (nth 3 (org-clock-get-nth-full-clock-data marker n)))

;;;###autoload
(defun org-clock-get-nth-half-clock-beginning (marker &optional n)
  (nth 0 (org-clock-get-nth-half-clock-data marker n)))
;;;###autoload
(defun org-clock-get-nth-half-clock-end (marker &optional n)
  (nth 1 (org-clock-get-nth-half-clock-data marker n)))
;;;###autoload
(defun org-clock-get-nth-half-clock-time (marker &optional n)
  (nth 2 (org-clock-get-nth-half-clock-data marker n)))

;; (org-clock-get-nth-clock-times org-clock-marker 1)



(progn

  (defun org-idle-tracing-function (orig-fun &rest args)
    (message "org-resolve-clocks-if-idle called with args %S" args)
    (let ((res (apply orig-fun args)))
      (message "org-resolve-clocks-if-idle returned %S" res)
      res))

  (advice-add 'org-resolve-clocks-if-idle :around #'org-idle-tracing-function))


;;;###autoload
(defun lotus-org-clock-detect-first-clockin-of-day ()
  ;; do necessary stuff
  ;; like context presentation etc.
  )

;;;###autoload
(defun lotus-org-clock-declare-last-clockout-of-day ()
  )

(defun lotus-org-clock-offline-time ()
;; clock the time from last known clock to now
  )

;;;###autoload
(defun lotus-org-schedule-deadline-with-eod ()
  ;; schedule deadline to honour end of day time also.
 )

;;;###autoload
(defun lotus-org-schedule-deadline-with-availability ()
  ;; calculate schedule deadline whether available in given time
 )

;;;###autoload
(defun org-define-a-task ()
  "This function is used to create a org tree to complete a task.
for e.g. implementing lvm support for guixsd what all steps a
person have to take he has to read scheme, guixsd details, than
see similar module and try to implement it."
  (interactive)
  )

;;;###autoload
(defun org-log-not-on-event (start end event)
  (ignore start)
  (ignore end)
  (ignore event))

;;;###autoload
(defun org-goto-refile (&optional refile-targets)
  "Refile goto."
  ;; mark paragraph if no region is set
  (let* ((org-refile-targets (or refile-targets org-refile-targets))
         (target (save-excursion (safe-org-refile-get-location)))
         (file (nth 1 target))
         (pos (nth 3 target)))
    (when (set-buffer (find-file-noselect file)) ;; (switch-to-buffer (find-file-noselect file) 'norecord)
      (goto-char pos))))

;;;}}}


;;;###autoload
(defun get-last-clock (tstart)
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (org-clock-sum tstart))))

;; (org-back-to-heading t)

;; macro org-with-silent-modifications

;; (org-element-type (org-element-context))
;; (eq (org-element-type (save-match-data (org-element-at-point))) 'clock)

;;; FOR WORKING FAST START CREATING TEMPLATE OR EMPTY FUNCTION BODY.


;;;###autoload
(defun kill-emacs-org-clock-out ()
  (when (org-clock-is-active)
    ;; (y-or-n-p-with-timeout (format "Do you want to clock out current task %s: " org-clock-heading) 7 nil)
    (org-with-clock-writeable
      (let (org-log-note-clock-out)
        (when (org-clock-is-active)
          (let* ((buff (marker-buffer org-clock-marker)))
            (org-clock-out)
            (when buff
              (with-current-buffer buff
                (org-lotus-modification-post-action)
                (save-buffer)))))))))

;;; org-clock-utils-lotus.el ends here
