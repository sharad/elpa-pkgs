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


(drive-extended@ @change-tansition (@transition-class))
(drive-extended@ @change-dectector (@transition-dectector-class))
(drive-extended@ @change-span-detector (@transition-span-dectector-class))


(defvar lotus-minimum-char-changes 70 "minimum char changes")
(defvar lotus-minimum-changes      70 "minimum changes")


(defun lotus-buffer-changes-count ()
  (let ((changes 0))
    (when buffer-undo-tree
      (undo-tree-mapc #'(lambda (node)
                          (ignore node)
                          (setq changes (+ changes 1)));; (length (undo-tree-node-next node))
                      (undo-tree-root buffer-undo-tree)))
    changes))

;; (undo-tree-mapc #'(lambda (n) (message "%s\n" n))
;;                 (undo-tree-root buffer-undo-tree))


(defvar lotus-last-buffer-undo-tree-count 0) ;internal add in session and desktop
(make-variable-buffer-local 'lotus-last-buffer-undo-tree-count)
(when (featurep 'desktop)
  (add-to-list 'desktop-locals-to-save 'lotus-last-buffer-undo-tree-count))
(when (featurep 'session)
  (add-to-list 'session-locals-include 'lotus-last-buffer-undo-tree-count))


(defun lotus-action-on-buffer-undo-tree-change (action
                                                buff
                                                &optional
                                                minimal-changes
                                                win-timeout)
  (if (eq buff (current-buffer))
      (with-current-buffer buff
        (let* ((minimal-changes (or minimal-changes
                                    lotus-minimum-char-changes))
               (win-timeout (or win-timeout 7))
               (totalchgcount (lotus-buffer-changes-count))
               (chgcount (- totalchgcount
                            lotus-last-buffer-undo-tree-count)))
          (if (>= chgcount
                  minimal-changes)
              (if (funcall action win-timeout
                           :buff
                           buff
                           :chgcount
                           chgcount
                           :success
                           #'(lambda ()
                               (with-current-buffer buff
                                 (setq lotus-last-buffer-undo-tree-count totalchgcount)))
                           :fail
                           #'(lambda ()
                               (with-current-buffer buff
                                 (setq lotus-last-buffer-undo-tree-count totalchgcount)))
                           :run-before nil)
                  (message "Lunched noter ret t")
                (message "Lunched noter ret nil"))
            (message "HELLO: buffer-undo-tree-change: only %d changes not more than %d" chgcount minimal-changes))))
    (message "HELLO Current buffer %s is not same as %s"
             (current-buffer)
             buff)))


(defvar lotus-last-buffer-undo-list-pos nil) ;internal add in session and desktop
(make-variable-buffer-local 'lotus-last-buffer-undo-list-pos)

;;;###autoload
(defun lotus-action-on-buffer-undo-list-change (action
                                                buff
                                                &optional
                                                minimal-char-changes
                                                win-timeout)
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
                         (cl-rest (memq lotus-last-buffer-undo-list-pos
                                        buffer-undo-list))
                         buffer-undo-list))
          undo)
      (while (and undo-list
                  (cl-first undo-list)
                  (< char-changes
                     minimal-char-changes))
        (setq undo (cl-first undo-list))
        (cond
          ((and (consp undo) (integerp (cl-first undo)) (integerp (cl-rest undo)))
           ;; (BEG . END)
           (setq char-changes (+ char-changes (abs (- (cl-first undo) (cl-rest undo))))))
          ((and (consp undo) (stringp (cl-first undo))) ; (TEXT . POSITION)
           (setq char-changes (+ char-changes (length (cl-first undo)))))
          ((and (consp undo) (eq (cl-first undo) t))) ; (t HIGH . LOW)
          ((and (consp undo) (null (cl-first undo))))
           ;; (nil PROPERTY VALUE BEG . END)
           ;; (setq position (rest (last undo)))

          ((and (consp undo) (markerp (cl-first undo)))) ; (MARKER . DISTANCE)
          ((integerp undo))               ; POSITION
          ((null undo))               ; nil
          (t (error "Invalid undo entry: %s" undo)))
        (setq undo-list (cl-rest undo-list)))

      (cond
        ((>= char-changes minimal-char-changes)
         (if (funcall action win-timeout
                      :buff
                      buff
                      :chgcount
                      char-changes
                      :success
                      #'(lambda ()
                          (with-current-buffer buff
                            (setq lotus-last-buffer-undo-list-pos undo)))
                      :fail
                      #'(lambda ()
                          (with-current-buffer buff
                            (setq lotus-last-buffer-undo-list-pos undo)))
                      :run-before nil)
             (setq lotus-last-buffer-undo-list-pos undo)))
        (t)))))


(defun org-onchange-register-in-session ()
  (when (featurep 'desktop)
    (add-hook 'desktop-locals-to-save 'lotus-last-buffer-undo-tree-count))
  (when (featurep 'session)
    (add-hook 'session-locals-include 'lotus-last-buffer-undo-tree-count))

  (when (featurep 'desktop)
    (add-hook 'desktop-locals-to-save 'lotus-last-buffer-undo-list-pos))
  (when (featurep 'session)
    (add-hook 'session-locals-include 'lotus-last-buffer-undo-list-pos)))

(defun org-onchange-unregister-in-session ()
  (when (featurep 'desktop)
    (remove-hook 'desktop-locals-to-save 'lotus-last-buffer-undo-tree-count))
  (when (featurep 'session)
    (remove-hook 'session-locals-include 'lotus-last-buffer-undo-tree-count))

  (when (featurep 'desktop)
    (remove-hook 'desktop-locals-to-save 'lotus-last-buffer-undo-list-pos))
  (when (featurep 'session)
    (remove-hook 'session-locals-include 'lotus-last-buffer-undo-list-pos)))



(defun detect-undo-changes-periodic-fn (&optional
                                        win-timeout)
  ;; (when (or t (eq buffer (current-buffer)))
  (let ((buff (current-buffer))
        (win-timeout (or win-timeout 7))
        (on-buffer-undo-chg-action (if (and (consp buffer-undo-list)
                                            (cl-first buffer-undo-list))
                                       #'lotus-action-on-buffer-undo-list-change
                                     #'lotus-action-on-buffer-undo-tree-change)))
    (funcall on-buffer-undo-chg-action
             #'org-clock-lotus-log-note-current-clock-with-timed-new-win
             buff
             lotus-minimum-char-changes
             win-timeout)))

(defvar detect-undo-changes-periodic-fn-timer nil
  "Time for on change log note.")

;;;###autoload
(defun detect-undo-changes-periodic-fn-start-timer (&optional
                                                    idle-timeout
                                                    win-timeout)
  (interactive)
  (let ((idle-timeout (or idle-timeout 10))
        (win-timeout  (or win-timeout   7)))
    (if detect-undo-changes-periodic-fn-timer
        (progn
          (cancel-timer detect-undo-changes-periodic-fn-timer)
          (setq detect-undo-changes-periodic-fn-timer nil)))
    (setq detect-undo-changes-periodic-fn-timer (run-with-idle-timer idle-timeout
                                                                        idle-timeout
                                                                        #'detect-undo-changes-periodic-fn (+ idle-timeout win-timeout)))))

;;;###autoload
(defun detect-undo-changes-periodic-fn-stop-timer ()
  (interactive)
  (if detect-undo-changes-periodic-fn-timer
      (progn
        (cancel-timer detect-undo-changes-periodic-fn-timer)
        (setq detect-undo-changes-periodic-fn-timer nil))))


;;;###autoload
(defun detect-undo-changes-periodic-fn-insinuate ()
  (interactive)
  ;; message-send-mail-hook
  (org-onchange-register-in-session)
  (detect-undo-changes-periodic-fn-start-timer 10 7))

;;;###autoload
(defun detect-undo-changes-periodic-fn-uninsinuate ()
  (interactive)
  ;; message-send-mail-hook
  (org-onchange-unregister-in-session)
  (detect-undo-changes-periodic-fn-stop-timer))
;; Org log note on change timer:1 ends here

;;; change-activity.el ends here
