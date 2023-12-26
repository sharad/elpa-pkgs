;;; org-onchange.el --- copy config         -*- lexical-binding: t; -*-

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
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
;;{{
     ;; https://emacs.stackexchange.com/questions/101/how-can-i-create-an-org-link-for-each-email-sent-by-mu4e
     ;; My first suggestion would be to try the following.

(add-hook 'message-send-hook #'(lambda ()
                                 (org-store-link nil)))

   ;; Since you said you tried the hook, another way is to just combine
   ;; org-store-link and message sending into a single function.

(defun store-link-then-send-message ()
  "Call `org-store-link', then send current email message."
  (interactive)
  (call-interactively #'org-store-link)
  (call-interactively #'message-send-and-exit))

(when (and (boundp 'mu4e-compose-mode-map)
           (keymapp mu4e-compose-mode-map))
  (define-key mu4e-compose-mode-map "\C-c\C-c" #'store-link-then-send-message)

  ;; This assumes you're using message-send-and-exit to send the message. You
  ;; could do something identical with the message-send command.

  (define-key mu4e-compose-mode-map "\C-c\C-c" #'store-link-then-send-message))
 ;;}}

 ;;{{ http://kitchingroup.cheme.cmu.edu/blog/2014/06/08/Better-integration-of-org-mode-and-email/
 ;; I like to email org-mode headings and content to people. It would be nice to
 ;; have some records of when a heading was sent, and to whom. We store this
 ;; information in a heading. It is pretty easy to write a simple function that
 ;; emails a selected region.

(defun email-region (start end)
  "Send region as the body of an email."
  (interactive "r")
  (let ((content (buffer-substring start end)))
    (compose-mail)
    (message-goto-body)
    (insert content)
    (message-goto-to)))

   ;; that function is not glamorous, and you still have to fill in the email
   ;; fields, and unless you use gnus and org-contacts, the only record keeping is
   ;; through the email provider.

   ;; What I would like is to send a whole heading in an email. The headline should
   ;; be the subject, and if there are TO, CC or BCC properties, those should be
   ;; used. If there is no TO, then I want to grab the TO from the email after you
   ;; enter it and store it as a property. You should be able to set OTHER-HEADERS
   ;; as a property (this is just for fun. There is no practical reason for this
   ;; yet). After you send the email, it should record in the heading when it was
   ;; sent.

   ;; It turned out that is a relatively tall order. While it is easy to setup the
   ;; email if you have everything in place, it is tricky to get the information on
   ;; TO and the time sent after the email is sent. Past lispers had a lot of ideas
   ;; to make this possible, and a day of digging got me to the answer. You can
   ;; specify some "action" functions that get called at various times, e.g. after
   ;; sending, and a return action when the compose window is done. Unfortunately,
   ;; I could not figure out any way to do things except to communicate through
   ;; some global variables.

   ;; So here is the code that lets me send org-headings, with the TO, CC, BCC
   ;; properties, and that records when I sent the email after it is sent.

(defvar *email-heading-point* nil
  "global variable to store point in for returning")

(defvar *email-to-addresses* nil
  "global variable to store to address in email")

(defun email-heading-return ()
  "after returning from compose do this"
  (switch-to-buffer (marker-buffer  *email-heading-point*))
  (goto-char (marker-position  *email-heading-point*))
  (setq *email-heading-point* nil)
  (org-set-property "SENT-ON" (current-time-string))
  ;; reset this incase you added new ones
  (org-set-property "TO" *email-to-addresses*))


(defun email-send-action ()
  "send action for compose-mail"
  (setq *email-to-addresses* (mail-fetch-field "To")))

(defun email-heading ()
  "Send the current org-mode heading as the body of an email, with headline as the subject.

     use these properties TO OTHER-HEADERS is an alist specifying
     additional header fields. Elements look like (HEADER . VALUE)
     where both HEADER and VALUE are strings.

     save when it was sent as s SENT property. this is overwritten on
     subsequent sends. could save them all in a logbook?
     "
  (interactive)
  ; store location.
  (setq *email-heading-point* (set-marker (make-marker) (point)))
  (org-mark-subtree)
  (let ((content (buffer-substring (point) (mark)))
        (TO (org-entry-get (point) "TO" t))
        (CC (org-entry-get (point) "CC" t))
        (BCC (org-entry-get (point) "BCC" t))
        (SUBJECT (nth 4 (org-heading-components)))
        (OTHER-HEADERS (eval (org-entry-get (point) "OTHER-HEADERS")))
        (continue nil)
        (switch-function nil)
        (yank-action nil)
        (send-actions '((email-send-action . nil)))
        (return-action '(email-heading-return)))

    (compose-mail TO SUBJECT OTHER-HEADERS continue switch-function yank-action send-actions return-action)
    (message-goto-body)
    (insert content)
    (when CC
      (message-goto-cc)
      (insert CC))
    (when BCC
      (message-goto-bcc)
      (insert BCC))
    (if TO
     (message-goto-body)
     (message-goto-to))))


   ;; This works pretty well for me. Since I normally use this to send tasks to
   ;; people, it keeps the task organized where I want it, and I can embed an
   ;; org-id in the email so if the person replies to it telling me the task is
   ;; done, I can easily navigate to the task to mark it off. Pretty handy.

   ;;}}
;; Org log note change from different sources:1 ends here

;; Provide this file

;; [[file:org-onchange.org::*Provide this file][Provide this file:1]]
(provide 'org-onchange)
;;; org-onchange.el ends here
;; Provide this file:1 ends here
