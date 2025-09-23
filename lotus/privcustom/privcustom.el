;;; privcustom.el --- pricate customizations         -*- lexical-binding: t; -*-

;; Copyright (C) 2025  sharad

;; Author: sharad <s@dell5480>
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

(provide 'privcustom)



;; -------------------------------
;; Private Customize System
;; -------------------------------

(defvar my-private-custom-file
  (expand-file-name "private-config.el" user-emacs-directory)
  "Separate file to store private customize settings.")

;; Ensure the file exists
(unless (file-exists-p my-private-custom-file)
  (with-temp-file my-private-custom-file
    (insert ";; Private custom settings\n")))

;; Load private settings
(load my-private-custom-file t)

;; Define a dedicated group
(defgroup my-private nil
  "Private configuration registry."
  :group 'convenience)

;; ;; Function to register/add a new private variable dynamically
;; (defun my-private-register (name &optional doc)
;;   "Register a new private config variable NAME with optional DOC.
;; Creates a defcustom dynamically in the `my-private` group."
;;   (interactive "sName of private config: \nsDescription (optional): ")
;;   (let* ((sym (intern (format "my-private-%s" name)))
;;          (docstring (or doc (format "Private config for %s." name))))
;;     (unless (custom-variable-p sym)
;;       (eval `(defcustom ,sym nil
;;                ,docstring
;;                :type 'string
;;                :group 'my-private)))
;;     (message "Registered %s in my-private group" sym)
;;     sym))

(defun my-private-register (name &optional doc)
  "Register a new private config variable NAME with optional DOC."
  (interactive "sName of private config: \nsDescription (optional): ")
  (let* ((sym (intern (format "my-private-%s" name)))
         (docstring (or doc (format "Private config for %s." name))))
    (unless (custom-variable-p sym)
      (eval `(defcustom ,sym nil
               ,docstring
               :type 'string
               :group 'my-private)))
    (message "Registered %s in my-private group" sym)
    ;; Open Customize to set value immediately
    (let ((custom-file my-private-custom-file))
      (customize-variable sym))
    sym))


;; Function to save all private variables to the separate file
(defun my-private-save ()
  "Save all `my-private` custom variables to private file."
  (interactive)
  (let ((custom-file my-private-custom-file))
    (custom-save-all)
    (message "Saved my-private variables to %s" custom-file)))

(defun my-private-browse ()
  "Browse all private config variables via Customize.
All changes are saved to `my-private-custom-file`."
  (interactive)
  (let ((custom-file my-private-custom-file))
    (customize-group 'my-private)))

;;; privcustom.el ends here
