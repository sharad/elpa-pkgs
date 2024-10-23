;;; org-onchange.el --- copy config         -*- lexical-binding: t; -*-

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
;; Provide this file

;; [[file:org-onchange.org::*Provide this file][Provide this file:1]]
(provide 'org-onchange)

;; Libraries required


;; [[file:org-onchange.org::*Libraries required][Libraries required:1]]
(require 'subr-x)
(require 'desktop)
(require 'session)

(require 'timer-utils-lotus)
(eval-when-compile
  (require 'timer-utils-lotus))
(require 'org-misc-utils-lotus)
(eval-when-compile
  (require 'org-misc-utils-lotus))
(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))
(eval-when-compile
  (require 'org-macs))
(eval-when-compile
  (require 'org-clock))
(require 'org-clock)
(require 'time-stamp)
(require 'undo-tree)
(require 'message)
(require 'org-insert-utils)
;; Libraries required:1 ends here


(defvar lotus-minimum-char-changes 70 "minimum char changes")
(defvar lotus-minimum-changes      70 "minimum changes")


;; *** Base functions
;; **** Note start
;; (defun org-add-note ()
;;   "Add a note to the current entry.
;;  This is done in the same way as adding a state change note."
;;   (interactive)
;;   (org-add-log-setup 'note))

;; **** org-add-log-setup (&optional purpose state prev-state how extra)
;; Set up the post command hook to take a note.
;; If this is about to TODO state change, the new state is expected in STATE.
;; HOW is an indicator what kind of note should be created.
;; EXTRA is additional text that will be inserted into the notes buffer.
;; #+BEGIN_SRC emacs-lisp :tangle no
;; (defun org-add-log-setup (&optional purpose state prev-state how extra) ...)

;; **** org-add-log-note (&optional _purpose)
;; Pop up a window for taking a note, and add this note later.
;; (setq-local org-finish-function 'org-store-log-note)
;; #+BEGIN_SRC elisp :tangle no
;; (defun org-add-log-note (&optional _purpose) ...)


;; **** org-store-log-note ()
;; Finish taking a log note, and insert it to where it belongs.
;; #+BEGIN_SRC elisp :tangle no
;; (defvar org-note-abort nil) ; dynamically scoped
;; (defun org-store-log-note ()


;; Org insert log note un-interactively


;; [[file:org-onchange.org::*Org insert log note un-interactively][Org insert log note un-interactively:1]]
;; copy of org-store-log-note


;; Org detect change to log note

;; *** Org detect change to log note
;; [[file:org-onchange.org::*Org detect change to log note][Org detect change to log note:1]]
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
(when (featurep 'desktop)
  (add-to-list 'desktop-locals-to-save 'lotus-last-buffer-undo-tree-count))
(when (featurep 'session)
  (add-to-list 'session-locals-include 'lotus-last-buffer-undo-tree-count))
(make-variable-buffer-local 'lotus-last-buffer-undo-tree-count)

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


(defun org-clock-lotus-log-note-on-change (&optional
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
;; Org detect change to log note:1 ends here

;; Org log note on change timer
;; *** Org log note on change timer
;; [[file:org-onchange.org::*Org log note on change timer][Org log note on change timer:1]]
(defvar org-clock-lotus-log-note-on-change-timer nil
  "Time for on change log note.")


;; (unintern 'org-clock-lotus-log-note-on-change-timer)

;;;###autoload
(defun org-clock-lotus-log-note-on-change-start-timer (&optional
                                                       idle-timeout
                                                       win-timeout)
  (interactive)
  (let ((idle-timeout (or idle-timeout 10))
        (win-timeout  (or win-timeout   7)))
    (if org-clock-lotus-log-note-on-change-timer
        (progn
          (cancel-timer org-clock-lotus-log-note-on-change-timer)
          (setq org-clock-lotus-log-note-on-change-timer nil)))
    (setq org-clock-lotus-log-note-on-change-timer (run-with-idle-timer idle-timeout
                                                                        idle-timeout
                                                                        #'org-clock-lotus-log-note-on-change (+ idle-timeout win-timeout)))))

;;;###autoload
(defun org-clock-lotus-log-note-on-change-stop-timer ()
  (interactive)
  (if org-clock-lotus-log-note-on-change-timer
      (progn
        (cancel-timer org-clock-lotus-log-note-on-change-timer)
        (setq org-clock-lotus-log-note-on-change-timer nil))))

;;;###autoload
(defun org-clock-lotus-log-note-on-change-insinuate ()
  (interactive)
  ;; message-send-mail-hook
  (org-onchange-register-in-session)
  (org-clock-lotus-log-note-on-change-start-timer 10 7))

;;;###autoload
(defun org-clock-lotus-log-note-on-change-uninsinuate ()
  (interactive)
  ;; message-send-mail-hook
  (org-onchange-unregister-in-session)
  (org-clock-lotus-log-note-on-change-stop-timer))
;; Org log note on change timer:1 ends here




;; Org log note change from different sources

;; [[file:org-onchange.org::*Org log note change from different sources][Org log note change from different sources:1]]
;; Org log note change from different sources:1 ends here

;;; org-onchange.el ends here
;; Provide this file:1 ends here
