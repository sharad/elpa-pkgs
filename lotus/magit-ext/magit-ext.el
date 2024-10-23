;;; magit-ext.el --- magit extentions                -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sharad

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

(provide 'magit-ext)


;;;###autoload
(defun magit-commit-with-single-line (msg &rest args)
  "Magit commit amend without editing."
  (interactive
   (list (read-from-minibuffer "Commit msg: " "correction")))
  (let ((msg (or msg "correction"))
        (default-directory (magit-toplevel)))
    (apply magit-call-git "commit" "-m" msg args)
    (magit-refresh)))

;; magit-run-git-with-editor

;;;###autoload
(defun magit-commit-amend-noedit ()
  "Magit commit amend without editing."
  (interactive)
  (magit-commit-amend '("--no-edit")))


;;;###autoload
(defun magit-push-current-force (target args)
  "Magit force push."
  (interactive
   (--if-let (magit-get-current-branch)
       (list (magit-read-remote-branch (format "Push %s to" it)
                                       nil nil it 'confirm)
             (magit-push-arguments))
     (user-error "No branch is checked out")))
  (magit-push-current target (cons "-f" args)))


;;;###autoload
(defun magit-commit-with-single-line-and-push (target args)
  "Magit commit amend without editing followed by force push."
  (interactive
   (--if-let (magit-get-current-branch)
       (list (magit-read-remote-branch (format "Push %s to" it)
                                       nil nil it 'confirm)
             (magit-push-arguments))
     (user-error "No branch is checked out")))
  (when (magit-commit-with-single-line)
    (magit-push-current target args)))

;;;###autoload
(defun magit-commit-amend-noedit-push-current-force (target args)
  "Magit commit amend without editing followed by force push."
  (interactive
   (--if-let (magit-get-current-branch)
       (list (magit-read-remote-branch (format "Push %s to" it)
                                       nil nil it 'confirm)
             (magit-push-arguments))
     (user-error "No branch is checked out")))
  (when (magit-commit-amend-noedit)
    (magit-push-current-force target args)))

;;; magit-ext.el ends here
