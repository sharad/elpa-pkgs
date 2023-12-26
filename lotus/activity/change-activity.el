;;; change-activity.el --- Emacs Change-Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Keywords: data

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

;; This package meant to log, analyze and report all emacs change-activity of
;; user which could further utilized to visualize change-activity of user
;; during period of time or editing session.

;; Enable Change-Activity for the current buffer by invoking
;; `change-activity-mode'. If you wish to activate it globally, use
;; `global-change-activity-mode'.

;; Set variable `change-activity-api-key' to your API key. Point
;; `change-activity-cli-path' to the absolute path of the CLI script
;; (change-activity-cli.py).

;;; Code:

(provide 'change-activity)


(eval-when-compile
  (require 'activity-macro))
(require 'activity-base)
(require 'org-insert-utils)


;; (defclass change-activity (buffer-activity)
;;   ((buffer :initarg :buffer
;;            :initform (current-buffer)
;;            :type buffer
;;            :documentation "Current buffer.")
;;    (marker :initarg :marker
;;            :initform (point-marker)
;;            :type marker
;;            :documentation "Current point marker."))
;;   "A buffer activity.")

(defun lotus-buffer-changes-count ()
  (let ((changes 0))
    (when buffer-undo-tree
      (undo-tree-mapc
       (lambda (node)
         (setq changes (+ changes 1;; (length (undo-tree-node-next node))
                          )))
       (undo-tree-root buffer-undo-tree)))
    changes))

(defvar lotus-minimum-char-changes 70)
(defvar lotus-minimum-changes 70)

(defvar lotus-last-buffer-undo-tree-count 0) ;internal add in session and desktop
(when (featurep 'desktop)
  (add-to-list 'desktop-locals-to-save 'lotus-last-buffer-undo-tree-count))
(when (featurep 'session)
  (add-to-list 'session-locals-include 'lotus-last-buffer-undo-tree-count))
(make-variable-buffer-local 'lotus-last-buffer-undo-tree-count)

(defvar lotus-last-buffer-undo-list-pos nil) ;internal add in session and desktop

(make-variable-buffer-local 'lotus-last-buffer-undo-list-pos)

(when (featurep 'desktop)
  (add-to-list 'desktop-locals-to-save 'lotus-last-buffer-undo-list-pos))
(when (featurep 'session)
  (add-to-list 'session-locals-include 'lotus-last-buffer-undo-list-pos))

 ;;;###autoload

(defun org-clock-lotus-log-note-current-clock-with-timed-new-win (win-timeout &optional fail-quietly)
  (interactive)
  (let ((win-timeout  (or win-timeout  7)))
    (when (org-clocking-p)
      (move-marker org-log-note-return-to (point))
      (org-clock-lotus-with-current-clock
          (org-add-log-setup-with-timed-new-win win-timeout
                                                'note nil nil nil
                                                (concat "# Task: " (org-get-heading t) "\n\n"))))))


(progn

 (defun lotus-action-on-buffer-undo-tree-change (action &optional minimal-changes win-timeout)
   (let ((win-timeout (or win-timeout 7))
         (chgcount (- (lotus-buffer-changes-count) lotus-last-buffer-undo-tree-count)))
     (if (>= chgcount minimal-changes)
         (if (funcall action win-timeout)
             (setq lotus-last-buffer-undo-tree-count chgcount))
         (when nil
           (@:message "TEST1: buffer-undo-tree-change: only %d changes not more than %d" chgcount minimal-changes)))))

 (defun lotus-action-on-buffer-undo-list-change (action &optional minimal-char-changes win-timeout)
   "Set point to the position of the last change.
 Consecutive calls set point to the position of the previous change.
 With a prefix arg (optional arg MARK-POINT non-nil), set mark so \
 \\[exchange-point-and-mark]
 will return point to the current position."
   ;; (interactive "P")
   ;; (unless (buffer-modified-p)
   ;;   (error "Buffer not modified"))
   (let ((win-timeout (or win-timeout 7)))
     (when (eq buffer-undo-list t)
       (error "No undo information in this buffer"))
     ;; (when mark-point (push-mark))
     (unless minimal-char-changes
       (setq minimal-char-changes 10))
     (let ((char-changes 0)
           (undo-list (if lotus-last-buffer-undo-list-pos
                          (cl-rest (memq lotus-last-buffer-undo-list-pos buffer-undo-list))
                          buffer-undo-list))
           undo)
       (while (and undo-list
                   (cl-first undo-list)
                   (< char-changes minimal-char-changes))
         (setq undo (cl-first undo-list))
         (cond
           ((and (consp undo) (integerp (cl-first undo)) (integerp (cl-rest undo)))
            ;; (BEG . END)
            (setq char-changes (+ char-changes (abs (- (cl-first undo) (cl-rest undo))))))
           ((and (consp undo) (stringp (cl-first undo))) ; (TEXT . POSITION)
            (setq char-changes (+ char-changes (length (cl-first undo)))))
           ((and (consp undo) (eq (cl-first undo) t))) ; (t HIGH . LOW)
           ((and (consp undo) (null (cl-first undo)))
            ;; (nil PROPERTY VALUE BEG . END)
            ;; (setq position (cl-rest (last undo)))
            )
           ((and (consp undo) (markerp (cl-first undo)))) ; (MARKER . DISTANCE)
           ((integerp undo))		; POSITION
           ((null undo))		; nil
           (t (error "Invalid undo entry: %s" undo)))
         (setq undo-list (cl-rest undo-list)))

       (cond
         ((>= char-changes minimal-char-changes)
          (if (funcall action win-timeout)
              (setq lotus-last-buffer-undo-list-pos undo)))
         (t ))))))

(defun org-clock-lotus-log-note-current-clock-with-timed-new-win (win-timeout &optional fail-quietly)
  (interactive)
  (let ((chgact nil))
    (setq chgact
          (change-activity "test"))))

(defun org-clock-lotus-log-note-on-change (&optional win-timeout)
  ;; (when (or t (eq buffer (current-buffer)))
  (let ((win-timeout (or win-timeout 7)))
    (if (and (consp buffer-undo-list)
             (cl-first buffer-undo-list))
        (lotus-action-on-buffer-undo-list-change #'org-clock-lotus-log-note-current-clock-with-timed-new-win  lotus-minimum-char-changes win-timeout)
        (lotus-action-on-buffer-undo-tree-change #'org-clock-lotus-log-note-current-clock-with-timed-new-win lotus-minimum-changes win-timeout))))

;;; change-activity.el ends here
