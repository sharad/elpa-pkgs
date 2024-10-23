;;; org-mail-misc.el --- org mail etc                -*- lexical-binding: t; -*-

;; Copyright (C) 2023  s

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(provide 'org-mail-misc)


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

;;; org-mail-misc.el ends here
