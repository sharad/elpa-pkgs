;;; mail-event.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: s <>
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

;; This package meant to log, analyze and report all emacs activity of
;; user which could further utilized to visualize activity of user
;; during period of time or editing session.

;; Enable Activity for the current buffer by invokingi
;; `activity-mode'. If you wish to activate it globally, use
;; `global-activity-mode'.

;; Set variable `activity-api-key' to your API key. Point
;; `activity-cli-path' to the absolute path of the CLI script
;; (activity-cli.py).

;; See http://nullprogram.com/blog/2013/04/07/ for help
;; add example code directly here for quick reference.

;;; Code:

(provide 'mail-event)


(require 'gnus)
(require 'org-capture-note)
(require 'message)



(eval-when-compile
  (require 'activity-macro))
(require 'activity)


;; https://emacs.stackexchange.com/questions/101/how-can-i-create-an-org-link-for-each-email-sent-by-mu4e
;; http://kitchingroup.cheme.cmu.edu/blog/2014/06/08/Better-integration-of-org-mode-and-email/
;; https://github.com/danieroux/emacs/blob/master/mine/djr-org-mu4e-capture-sent.el
;; TODO: see it https://orgmode.org/manual/Template-expansion.html#Template-expansion

(defun activity~wipe-brackets (msgid)
  (interactive)
  (cl-remove-if #'(lambda (c)
                    (or (equal c ?>)
                        (equal c ?<)))
                msgid))

(defun lotus-plist-get-members (plist keys)
  (mapcar #'(lambda (k)
              (plist-get plist k))
          keys))


(drive-extended@ @mail-read-event-detector (@event-dectector-class)
  (def@ @@ :make-message ()
    (let* ((msgid   (message-fetch-field "Message-ID" t))
           (subject (message-fetch-field "Subject" t))
           (from    (message-fetch-field "From" t))
           (to      (message-fetch-field "To" t))
           (link    (concat "mu4e:msgid:" (activity~wipe-brackets msgid))))
      (ignore link)
      (list :subject subject
            :from from
            :to to)))
  (def@ @@ :make-event ()
    "Make mail read event."
    (let* ((note (@! @:note :new)))
      (@:message "processing %s" (@:make-message))
      (@! note :send
          '(clock)
          (apply #'format (string-join '("* Reading mail subject: %s" "from: %s" "to: %s") "\n")
                 (lotus-plist-get-members (@:make-message) '(:subject :from :to))))))
  (def@ @@ :make-event-gnus ()
    (when (and gnus-article-buffer
               (get-buffer gnus-article-buffer))
      (with-current-buffer (get-buffer gnus-article-buffer)
        (let ((subject (message-fetch-field "Subject" t)))
          (@:message "checking: %s" subject)
          (@:make-event)))))
  (def@ @@ :dispatch ()
    "setting note class"
    (setf @:note @org-capture-edit-entry-dest-note))

  (def@ @@ :initialize ()
    "setting note class"
    (setf @:note @org-capture-edit-entry-dest-note))
  ;; gnus-Article-prepare-hook
  ;; gnus-Select-article-hook
  ;; (add-hook
  ;;  'gnus-article-prepare-hook
  ;;  (lambda () (@! @@ :make-event-gnus)))
  (@:dispatch))


(drive-extended@ @mail-send-event-detector (@event-dectector-class)
  (def@ @@ :make-message ()
    (let* ((msgid   (message-fetch-field "Message-ID" t))
           (subject (message-fetch-field "Subject" t))
           (from    (message-fetch-field "From" t))
           (to      (message-fetch-field "To" t))
           (link    (concat "mu4e:msgid:" (activity~wipe-brackets msgid))))
      (ignore link)
      (list :subject subject
            :from    from
            :to      to)))
  (def@ @@ :make-event ()
    "Make mail send event."
    (let ((note (@! @:note :new)))
      (@! note :send
          '(clock)
          (apply #'format (string-join '("* Sent mail subject: %s" "to: %s") "\n")
                 (lotus-plist-get-members (@:make-message) '(:subject :to))))))
  (def@ @@ :make-event-gnus ()
    (when (and gnus-message-buffer
               (get-buffer gnus-message-buffer))
      (with-current-buffer (get-buffer gnus-message-buffer)
        (let ((subject
               (message-fetch-field "Subject" t)))
          (@:message "sending mail: %s" subject)
          (@:make-event)))))
  (def@ @@ :dispatch ()
    "setting note class"
    (setf @:note @org-capture-edit-entry-dest-note))

  (def@ @@ :initialize ()
    "setting note class"
    (setf @:note @org-capture-edit-entry-dest-note))

  (@:dispatch))


(defvar @mail-read-event-detector-instance nil)

(defun mail-event-run-action ()
  (run-with-timer nil nil
                  ;; gnus-article-prepare-hook neeed to be finished before any interactive command
                  #'(lambda ()
                      (@! @mail-read-event-detector :make-event-gnus))))


(drive-extended@ @mail-event-activity (@activity-interface)
  (def@ @@ :key ()
    "mail-event-activity"
    "mail-event-activity")
  (def@ @@ :activate ()
    (add-hook 'gnus-article-prepare-hook
              #'mail-event-run-action))
  (def@ @@ :deactivate ()
    (remove-hook 'gnus-article-prepare-hook
                 #'mail-event-run-action))

  (def@ @@ :initialize ()
    ))

;; (defun mail-event-activity ()
;;   @mail-event-activity)

;; (defun global-activity-mode-enable-in-buffers ())

;;;###autoload
(defun activity-register-mail-event ()
  (interactive)
  (activity-register @mail-event-activity))

;;;###autoload
(add-hook 'activity-register-hook
          #'activity-register-mail-event)

;;; mail-event.el ends here
