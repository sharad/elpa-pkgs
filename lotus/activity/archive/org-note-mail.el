;;; org-note-mail.el --- copy config         -*- lexical-binding: t; -*-

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

;; https://github.com/danieroux/emacs/blob/master/mine/djr-org-mu4e-capture-sent.el

;;; Store emails that are send/forwarded/replied to

;; Invoking lotus-org-mu4e-capture-next-message or setting
;; lotus-org-mu4e-must-capture-message before composing a message will call
;; org-capture after the email was sent successfully (using the capture template
;; from lotus-mu4e-org-mode-capture-template-for-sent-email)

;;; Code:

;; Libraries required


;; [[file:org-note-mail.org::*Libraries required][Libraries required:1]]
(defvar lotus-org-mu4e-must-capture-message nil
  "If set, the next composed mu4e message will automatically be captured with the template specified in lotus-mu4e-org-mode-capture-template-for-sent-email")

(defvar lotus-mu4e-captured-message-p nil
  "Plist with info about the most recently sent mu4e email for OrgMode purposes")

(defvar lotus-mu4e-org-mode-capture-template-for-sent-email "e"
  "The specific template from org-capture-templates to use when capturing a sent email automatically")

(add-hook 'message-sent-hook 'lotus-org-mu4e-store-link-on-sent-message)

(add-hook 'message-mode-hook (lambda ()
			       (message-add-action 'lotus-org-mu4e-capture-cancel
						   'send 'postpone 'kill)

			       (message-add-action 'lotus-capture-sent-message-if-needed
						   'send)))

(defun djr~wipe-brackets (msgid)
  (interactive)
  (remove-if (lambda (c)
	       (or (equal c ?>)
		   (equal c ?<)))
	     msgid))

(defun lotus-org-mu4e-store-link-on-sent-message ()
  "Store the sent message in many useful places"
  (interactive)
  (let* ((msgid (message-fetch-field "Message-ID"))
	 (description (message-fetch-field "Subject"))
	 (link (concat "mu4e:msgid:" (djr~wipe-brackets msgid)))
	 (org-link-string (org-make-link-string link description))
	 (captured-message-p
	  `(:type mu4e
		 :description ,description
		 :link ,link
		 :annotation ,org-link-string
		 :message-id ,msgid))
	 (stored-link (list link description)))
    (push stored-link org-stored-links)
    (setq org-store-link-plist captured-message-p
	  lotus-mu4e-captured-message-p org-store-link-plist)))

(defun lotus-capture-sent-message-if-needed ()
  (interactive)
  (if lotus-org-mu4e-must-capture-message
      (let* ((org-store-link-plist lotus-mu4e-captured-message-p)
             (org-capture-link-is-already-stored t))
        (org-capture nil lotus-mu4e-org-mode-capture-template-for-sent-email))))

(defun lotus-org-mu4e-capture-cancel ()
  (interactive)
  (setq lotus-org-mu4e-must-capture-message nil
	global-mode-string (delq 'djr-org-capture-mode-line-string global-mode-string)))
(lotus-org-mu4e-capture-cancel)

(defun lotus-org-mu4e-capture-next-message ()
  (setq lotus-org-mu4e-must-capture-message t
	djr-org-capture-mode-line-string "Org capturing current mail")
  (or global-mode-string (setq global-mode-string '("")))
  (or (memq 'djr-org-capture-mode-line-string global-mode-string)
      (setq global-mode-string
	    (append global-mode-string '(djr-org-capture-mode-line-string)))))

(defun lotus-mu4e-compose-new-with-follow-up ()
  (interactive)
  (lotus-org-mu4e-capture-next-message)
  (mu4e-compose-new))

(defun lotus-mu4e-compose-reply-with-follow-up ()
  (interactive)
  (lotus-org-mu4e-capture-next-message)
  (mu4e-compose-reply))

(defun lotus-mu4e-forward-with-follow-up ()
  (interactive)
  (lotus-org-mu4e-capture-next-message)
  (mu4e-compose-forward))
;; Libraries required:1 ends here

;; Org insert log note un-interactively


;; [[file:org-note-mail.org::*Org insert log note un-interactively][Org insert log note un-interactively:1]]

;; Org insert log note un-interactively:1 ends here

;; Clock out with NOTE


;; [[file:org-note-mail.org::*Clock out with NOTE][Clock out with NOTE:1]]

;; Clock out with NOTE:1 ends here

;; Org add log note with-timed-new-win
;; background in name is misleading it at present log-note show org file buffer to
;; add note but in this case it is not shown so background word is used.

;; *Note:* these function prepare buffer or window (timed) to take log note
;;  main work is only done by _org-store-log-note_


;; [[file:org-note-mail.org::*Org add log note with-timed-new-win][Org add log note with-timed-new-win:1]]

;; Org add log note with-timed-new-win:1 ends here

;; Org detect change to log note


;; [[file:org-note-mail.org::*Org detect change to log note][Org detect change to log note:1]]

;; Org detect change to log note:1 ends here

;; Org log note on change timer

;; [[file:org-note-mail.org::*Org log note on change timer][Org log note on change timer:1]]

;; Org log note on change timer:1 ends here

;; Org log note change from different sources

;; [[file:org-note-mail.org::*Org log note change from different sources][Org log note change from different sources:1]]

;; Org log note change from different sources:1 ends here

;; Provide this file

;; [[file:org-note-mail.org::*Provide this file][Provide this file:1]]
(provide 'org-note-mail)
;;; org-note-mail.el ends here
;; Provide this file:1 ends here
