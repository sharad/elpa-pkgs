;;; kontribute.el --- A package to facilitate contributions to projects following the Kontribute standard -*- lexical-binding: t; -*-

;; Copyright (C) 2025  sharad

;; Author: sharad <spratap@merunetworks.com>
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
;; This package provides an Emacs interface to interact with projects adhering to the Kontribute standard.
;; It helps developers locate project details and take necessary actions such as cloning repositories,
;; switching branches, and contacting project communication channels.

;;

;;; Code:

(provide 'kontribute)


(require 'json)
(require 'url)
(require 'url-http)

(defvar kontribute-standard-file "kontribute.json"
  "The default name of the Kontribute standard file.")

(defun kontribute-parse-standard (file-path)
  "Parse the Kontribute standard file from FILE-PATH and return its data as a plist."
  (let ((json-object-type 'plist))
    (json-read-file file-path)))

(defun kontribute-checkout-repo (repo-url branch)
  "Clone the repository from REPO-URL and switch to the specified BRANCH."
  (let ((default-directory (read-directory-name "Select target directory: ")))
    (shell-command (format "git clone %s" repo-url))
    (let ((repo-name (file-name-base repo-url)))
      (let ((repo-path (expand-file-name repo-name default-directory)))
        (if (file-directory-p repo-path)
            (progn
              (message "Cloning successful: %s" repo-path)
              (when branch
                (shell-command (format "cd %s && git checkout %s" repo-path branch))))
          (error "Failed to clone repository"))))))

(defun kontribute-contact-mailing-list (email)
  "Compose an email to the specified EMAIL."
  (compose-mail email))

(defun kontribute-contact-channel (channel-url)
  "Open the messaging CHANNEL-URL in the default browser."
  (browse-url channel-url))

(defun kontribute-init ()
  "Initialize the Kontribute interaction."
  (interactive)
  (let* ((file-path (read-file-name "Select the Kontribute standard file: "))
         (standard-data (kontribute-parse-standard file-path))
         (repo-url (plist-get standard-data :repository))
         (branch (plist-get standard-data :branch))
         (mailing-list (plist-get standard-data :mailinglist))
         (messaging-channel (plist-get standard-data :messaging-channel)))
    (when repo-url
      (kontribute-checkout-repo repo-url branch))
    (when (yes-or-no-p "Do you want to contact the mailing list? ")
      (kontribute-contact-mailing-list mailing-list))
    (when (yes-or-no-p "Do you want to join the messaging channel? ")
      (kontribute-contact-channel messaging-channel))))

;;; kontribute.el ends here
