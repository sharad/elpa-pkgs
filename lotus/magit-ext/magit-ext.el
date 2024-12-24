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
    (apply #'magit-call-git "commit" "-m"
           msg
           args)))

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
                                       nil ;; (magit-get "branch" it "remote")
                                       (magit-get-upstream-branch)
                                       it 'confirm)
             (magit-push-arguments))
     (user-error "No branch is checked out")))
  (magit-push-current target (cons "-f" args)))


;;;###autoload
(defun magit-commit-with-single-line-and-push (msg target args)
  "Magit commit amend without editing followed by force push."
  (interactive
   (--if-let (magit-get-current-branch)
       (list (read-from-minibuffer "Commit msg: " "correction")
             (magit-read-remote-branch (format "Push %s to" it)
                                       nil ;; (magit-get "branch" it "remote")
                                       (magit-get-upstream-branch)
                                       it 'confirm)
             (magit-push-arguments))
     (user-error "No branch is checked out")))
  (--when-let (magit-commit-with-single-line msg)
    (magit-push-current target args)
    (magit-refresh)))

;;;###autoload
(defun magit-commit-amend-noedit-push-current-force (target args)
  "Magit commit amend without editing followed by force push."
  (interactive
   (--if-let (magit-get-current-branch)
       (list (magit-read-remote-branch (format "Push %s to" it)
                                       nil ;; (magit-get "branch" it "remote")
                                       (magit-get-upstream-branch)
                                       it 'confirm)
             (magit-push-arguments))
     (user-error "No branch is checked out")))
  (when (magit-commit-amend-noedit)
    (magit-push-current-force target args)))


;; https://emacs.stackexchange.com/questions/26579/how-to-extend-magits-context-sensitive-push-menu
;; https://github.com/magit/forge
;; see forge.el
;; (magit-define-popup-action 'magit-push-popup
;;   ?!
;;   "Make remotely"
;;   'aec/ssh-make-and-fetch)
;;
;; (transient-append-suffix 'magit-push
;;   "m"
;;   '"!"
;;   "Make remotely"
;;   'aec/ssh-make-and-fetch)

(transient-append-suffix 'magit-commit
  "c" '("F" "Fast commit push" magit-commit-with-single-line-and-push))





(transient-define-prefix gita-transient ()
  "Transient menu for Gita commands."
  [["Basic Commands"
    ("s" "Status" gita-status)
    ("p" "Push" gita-push)
    ("f" "Fetch" gita-fetch)]
   ["Advanced Commands"
    ("r" "Rebase" gita-rebase)
    ("c" "Commit" gita-commit)
    ("l" "Log" gita-log)]
   ["Miscellaneous"
    ("d" "Diff" gita-diff)
    ("x" "Reset" gita-reset)]])


;; (transient-append-suffix 'magit-push
;;   "e" '("C" "AAA" magit-commit-amend-noedit-push-current-force))

;; (transient-append-suffix 'magit-fetch "m" '("n" forge-pull))
;; (transient-append-suffix 'magit-fetch "n" '("N" forge-pull-notifications))


;; (transient-define-prefix magit-ext-action ()
;;   "Different git actions."
;;   :info-manual "(magit)Initiating a Commit"
;;   :man-page "git-commit"
;;   ["Arguments"
;;    ("-a" "Stage all modified and deleted files"   ("-a" "--all"))
;;    ("-e" "Allow empty commit"                     "--allow-empty")
;;    ("-v" "Show diff of changes to be committed"   ("-v" "--verbose"))
;;    ("-n" "Disable hooks"                          ("-n" "--no-verify"))
;;    ("-R" "Claim authorship and reset author date" "--reset-author")
;;    (magit:--author :description "Override the author")
;;    (7 "-D" "Override the author date" "--date=" transient-read-date)
;;    ("-s" "Add Signed-off-by line"                 ("-s" "--signoff"))
;;    (5 magit:--gpg-sign)
;;    (magit-commit:--reuse-message)]
;;   [["Create"
;;     ("c" "Commit"         magit-commit-create)]
;;    ["Edit HEAD"
;;     ("e" "Extend"         magit-commit-extend)
;;     ("w" "Reword"         magit-commit-reword)
;;     ("a" "Amend"          magit-commit-amend)
;;     (6 "n" "Reshelve"     magit-commit-reshelve)]
;;    ["Edit"
;;     ("f" "Fixup"          magit-commit-fixup)
;;     ("s" "Squash"         magit-commit-squash)
;;     ("A" "Augment"        magit-commit-augment)
;;     (6 "x" "Absorb changes" magit-commit-autofixup)
;;     (6 "X" "Absorb modules" magit-commit-absorb-modules)]
;;    [""
;;     ("F" "Instant fixup"  magit-commit-instant-fixup)
;;     ("S" "Instant squash" magit-commit-instant-squash)]]
;;   (interactive)
;;   (if-let ((buffer (magit-commit-message-buffer)))
;;       (switch-to-buffer buffer)
;;     (transient-setup 'magit-ext-action)))

;; (transient-define-prefix magit-ext-action1 ()
;;   "Fetch from another repository."
;;   :man-page "git-fetch"
;;   ["Arguments"
;;    ("-p" "Prune deleted branches" ("-p" "--prune"))
;;    ("-t" "Fetch all tags" ("-t" "--tags"))
;;    ("-u" "Fetch full history" "--unshallow" :level 7)
;;    ("-F" "Force" ("-f" "--force"))]
;;   ["Fetch from"
;;    ("p" magit-fetch-from-pushremote)
;;    ("u" magit-fetch-from-upstream)
;;    ("e" "elsewhere"        magit-fetch-other)
;;    ("a" "all remotes"      magit-fetch-all)]
;;   ["Fetch"
;;    ("o" "another branch"   magit-fetch-branch)
;;    ("r" "explicit refspec" magit-fetch-refspec)
;;    ("m" "submodules"       magit-fetch-modules)]
;;   ["Configure"
;;    ("C" "variables..." magit-branch-configure)])


;; ;; magit-status-mode-map

;; (defvar forge-add-default-sections t
;;   "Whether to add Forge's sections to `magit-status-sections-hook'.

;; If you want to disable this, then you must set this to nil before
;; `forge' is loaded.")

;; (when forge-add-default-sections
;;   (magit-add-section-hook 'magit-status-sections-hook #'forge-insert-pullreqs nil t)
;;   (magit-add-section-hook 'magit-status-sections-hook #'forge-insert-issues   nil t))

;; ;;;###autoload
;; (defvar forge-add-default-bindings t
;;   "Whether to add Forge's bindings to various Magit keymaps.

;; If you want to disable this, then you must set this to nil before
;; `magit' is loaded.  If you do it before `forge' but after `magit'
;; is loaded, then `magit-mode-map' ends up being modified anyway.")

;; ;;;###autoload
;; (with-eval-after-load 'magit-mode
;;   (when forge-add-default-bindings
;;     (keymap-set magit-mode-map "'" #'forge-dispatch)
;;     (keymap-set magit-mode-map "N" #'forge-dispatch)
;;     (keymap-set magit-mode-map "<remap> <magit-browse-thing>"
;;                 #'forge-browse)
;;     (keymap-set magit-mode-map "<remap> <magit-copy-thing>"
;;                 #'forge-copy-url-at-point-as-kill)))

;; ;;;###autoload
;; (with-eval-after-load 'git-commit
;;   (when forge-add-default-bindings
;;     (keymap-set git-commit-mode-map "C-c C-v" #'forge-visit-topic)))

;; (when forge-add-default-bindings
;;   (keymap-set magit-commit-section-map "C-c C-v" #'forge-visit-topic)
;;   (keymap-set magit-branch-section-map "C-c C-v" #'forge-visit-topic)

;;   (transient-insert-suffix 'magit-dispatch "o"
;;     '("N" "Forge" forge-dispatch))

;;   (transient-append-suffix 'magit-fetch "m" '("n" forge-pull))
;;   (transient-append-suffix 'magit-fetch "n" '("N" forge-pull-notifications))

;;   (transient-append-suffix 'magit-pull  "m" '("n" forge-pull))
;;   (transient-append-suffix 'magit-pull  "n" '("N" forge-pull-notifications))

;;   (transient-append-suffix 'magit-branch "w"
;;     '("f" "pull-request" forge-checkout-pullreq))
;;   (transient-append-suffix 'magit-branch "W"
;;     '("F" "from pull-request" forge-branch-pullreq))

;;   (transient-suffix-put 'magit-remote 'magit-update-default-branch :key "b u")
;;   (transient-append-suffix 'magit-remote "b u"
;;     '("b r" "Rename default branch" forge-rename-default-branch))

;;   (transient-append-suffix 'magit-worktree "c"
;;     '("n" "pull-request worktree" forge-checkout-worktree))

;;   (transient-append-suffix 'magit-status-jump "w"
;;     '("Np" "Pull requests" forge-jump-to-pullreqs))
;;   (transient-append-suffix 'magit-status-jump "Np"
;;     '("Ni" "Issues" forge-jump-to-issues))

;;   (transient-append-suffix 'magit-merge "a"
;;     '(7 "M" "Merge using API" forge-merge)))


;; ;; (transient-append-suffix 'magit-commit "n" '("N" forge-pull-notifications))




;;; magit-ext.el ends here
