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
(defvar magit-wip-push-mode-after-success-hook nil)
(defvar magit-wip-push-mode-after-fail-hook nil)

(defun magit-wip-ext-push-commits-behind-count (ref &optional remote args)
  (interactive (list (magit-wip-get-ref)
                     nil
                     nil))
  (let* ((local-branch    (substring ref (length "refs/heads/")))
         (upstream-remote (or remote
                              (magit-get-upstream-remote local-branch)))
         (wip-ref         (string-join (list "wip/wtree" ref) "/"))
         (local-wip-ref   (string-join (list "refs" wip-ref) "/"))
         (remote-wip-branch  (string-join (list upstream-remote wip-ref) "/")))
    (if (magit-ref-p remote-wip-branch)
        (let ((count (string-to-number (magit-git-string "rev-list" "--count"
                                                         (concat remote-wip-branch
                                                                 ".."
                                                                 local-wip-ref)))))
          (if (call-interactively)
              (message "remote: %s is behind by %d"
                       remote-wip-branch
                       count)
            count))
      (if (call-interactively)
          (message "remote: %s not exists" remote-wip-branch)
        t))))

(defun magit-wip-ext-can-push-p (ref &optional remote args)
  (let ((count (magit-wip-ext-push-commits-behind-count ref remote args)))
    (if (eq count t)
        t
      (> count magit-wip-push-inhibit-count))))

(defun magit-ext-git-push-nons (branch target args)
  (run-hooks 'magit-credential-hook)
  ;; If the remote branch already exists, then we do not have to
  ;; qualify the target, which we prefer to avoid doing because
  ;; using the default namespace is wrong in obscure cases.
  (pcase-let ((namespace (if (magit-get-tracked target) "" "refs/heads/"))
              (`(,remote . ,target)
               (magit-split-branch-name target)))
    (magit-run-git-async "push" "-v" args remote
                         (format "%s:%s" branch target))))

(defun magit-wip-push (ref &optional remote args)
  (interactive
   (list (magit-ref-fullname "HEAD")
         (magit-get-upstream-remote (magit-get-current-branch))
         (when current-prefix-arg '("-f"))))
  (let* ((local-branch    (substring ref (length "refs/heads/")))
         (upstream-remote (or remote
                              (magit-get-upstream-remote local-branch)))
         (wip-ref         (string-join (list "wip/wtree" ref) "/"))
         (local-wip-ref   (string-join (list "refs" wip-ref) "/"))
         (remote-wip-ref  (string-join (list "refs/heads" wip-ref) "/")))
    ;; (message "magit-wip-push: wip-ref: %s" wip-ref)
    ;; (message "magit-wip-push: local-wip-ref: %s" local-wip-ref)
    ;; (message "magit-wip-push: upstream-remote: %s" upstream-remote)
    ;; (message "magit-wip-push: remote-wip-ref: %s" remote-wip-ref)
    (if (magit-ref-p local-wip-ref) ;(magit-wip-commit nil "wip-push-save tracked files")
        (if (magit-ext-git-push-nons local-wip-ref
                                     (string-join (list upstream-remote remote-wip-ref) "/")
                                     args)
            (progn
              (message "magit-wip-push: push passed")
              (run-hooks magit-wip-push-mode-after-success-hook))
          (progn
            (message "magit-wip-push: push failed")
            (run-hooks magit-wip-push-mode-after-fail-hook)))
      (message "magit-wip-push: ref %s not exists"
               local-wip-ref))))
(defun magit-wip-commit-worktree-fn-to-push-wip (ref files msg)
  (message "magit-wip-push: ref %s, files %s, msg %s"
           ref files msg)
  (if (magit-wip-ext-can-push-p ref)
      (magit-wip-push ref)
    (message "Inhibiting to push.")))
(defun magit-wip-commit-worktree-around-advice-fn (orgfn &rest args)
  (if (apply orgfn args)
      (message "magit-wip-commit: success")
    (message "magit-wip-commit: fail"))
  (message "magit-wip-push: args: %S" args)
  (apply #'magit-wip-commit-worktree-fn-to-push-wip
         args))


;; Define the global minor mode for magit wip push
;;;###autoload
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
