;;; magit-wip-ext.el --- magit wip extention         -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Music Player Daemon (MPD) user

;; Author: Music Player Daemon (MPD) user <spratap@merunetworks.com>
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

(provide 'magit-wip-ext)


(defvar magit-wip-push-inhibit-count 10)
(defun magit-wip-push (ref &optional remote args)
  (interactive
   (list (magit-ref-fullname "HEAD")
         (magit-get-upstream-remote (magit-get-current-branch))
         (when current-prefix-arg '("-f"))))
  (let* ((local-branch    (substring ref (length "refs/heads/")))
         (upstream-remote (or remote
                              (magit-get-upstream-remote local-branch)))
         (wip-ref (concat "wip/wtree/" ref))
         (local-wip-ref (string-join (list "refs" wip-ref) "/")))
    (if (magit-ref-p local-wip-ref)
        (if (magit-git-push local-wip-ref
                            (string-join (list upstream-remote wip-ref) "/")
                            args)
            (message "magit-wip-push: push passed")
          (message "magit-wip-push: push failed"))
      (message "magit-wip-push: ref %s not exists"
               wip-ref))))
(defun magit-wip-commit-worktree-fn-to-push-wip (ref files msg)
  (message "magit-wip-push: ref %s, files %s, msg %s"
           ref files msg)
  (magit-wip-push ref))
(defun magit-wip-commit-worktree-around-advice-fn (orgfn &rest args)
  (if (apply orgfn args)
      (message "magit-wip-push: success")
    (message "magit-wip-push: fail"))
  (message "magit-wip-push: args: %S" args)
  (apply #'magit-wip-commit-worktree-fn-to-push-wip
         args))
;; Define the global minor mode for magit wip push
(define-minor-mode magit-wip-push-mode
  "A global minor mode magit wip push."
  :global t                             ; Make it a global minor mode
  :init-value nil                       ; Default to disabled
  :lighter " WP"                        ; Display in the mode line
  (if magit-wip-push-mode
      (advice-add 'magit-wip-commit-worktree
                  :around #'magit-wip-commit-worktree-around-advice-fn)
    (when (or t (advice--p #'magit-wip-commit-worktree))
      (advice-remove 'magit-wip-commit-worktree
                     #'magit-wip-commit-worktree-around-advice-fn))))

;;; magit-wip-ext.el ends here
