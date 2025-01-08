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


(require 'transient)


;; Configuring --no-edit for Amend by Default

;; If you often want to amend with --no-edit, you can add this behavior to Magit’s amend command by customizing it:

;; Add the following to your Emacs configuration file:

;; (transient-append-suffix 'magit-commit "a"
;;   '("e" "Amend w/ --no-edit" magit-commit-amend "--no-edit"))


(defvar magit-single-line-fast-commit-msg "correction")

;;;###autoload
(defun magit-commit-with-single-line (msg &rest args)
  "Magit commit amend without editing."
  (interactive
   (list (read-from-minibuffer "Commit msg: "
                               magit-single-line-fast-commit-msg)))
  (or (magit-toplevel)
      (magit--not-inside-repository-error))
  (let ((msg (or msg magit-single-line-fast-commit-msg))
        (default-directory (magit-toplevel)))
    (when current-prefix-arg
      (magit-stage-modified t))
    (apply #'magit-call-git "commit" "-m"
           msg
           args)))

;;;###autoload
(defun magit-commit-with-single-line-fast (&rest args)
  "Magit commit amend without editing."
  (interactive)
  (let ((msg magit-single-line-fast-commit-msg)
        (default-directory (magit-toplevel)))
    (magit-commit-with-single-line msg)))


;;;###autoload
(defun magit-stage-and-commit-with-single-line (msg &rest args)
  "Magit commit amend without editing."
  (interactive
   (list (read-from-minibuffer "Commit msg: "
                               magit-single-line-fast-commit-msg)))
  (let ((msg (or msg magit-single-line-fast-commit-msg))
        (default-directory (magit-toplevel)))
    (magit-stage-modified t)
    (magit-commit-with-single-line msg)))
;;;###autoload
(defun magit-stage-and-commit-with-single-line-fast (&rest args)
  "Magit commit amend without editing."
  (interactive)
  (let ((msg magit-single-line-fast-commit-msg)
        (default-directory (magit-toplevel)))
    (magit-stage-modified t)
    (magit-commit-with-single-line msg)))


;;;###autoload
(defun magit-commit-with-single-line-and-push (msg target &rest args)
  "Magit commit amend without editing followed by force push."
  (interactive
   (--if-let (magit-get-current-branch)
       (list (read-from-minibuffer "Commit msg: "
                                   magit-single-line-fast-commit-msg)
             (magit-read-remote-branch (format "Push %s to" it)
                                       nil ;; (magit-get "branch" it "remote")
                                       (magit-get-upstream-branch)
                                       it 'confirm)
             (magit-push-arguments))
     (user-error "No branch is checked out")))
  (--when-let (magit-commit-with-single-line msg)
    (magit-push-current target args)))

;;;###autoload
(defun magit-commit-with-single-line-and-push-fast (msg &rest args)
  "Magit commit single line msg and push."
  (interactive (list (read-from-minibuffer "Commit msg: "
                                           magit-single-line-fast-commit-msg)))
  (let ((msg msg)
        (target (magit-get-upstream-branch)))
    (magit-commit-with-single-line-and-push msg
                                            target)))

;;;###autoload
(defun magit-commit-correction-fast (&rest args)
  "Magit commit corection fast."
  (interactive)
  (magit-commit-with-single-line-and-push-fast magit-single-line-fast-commit-msg))


;;;###autoload
(defun magit-stage-and-commit-with-single-line-and-push (msg target args)
  "Magit commit amend without editing followed by force push."
  (interactive
   (--if-let (magit-get-current-branch)
       (list (read-from-minibuffer "Commit msg: " msg)
             (magit-read-remote-branch (format "Push %s to" it)
                                       nil ;; (magit-get "branch" it "remote")
                                       (magit-get-upstream-branch)
                                       it 'confirm)
             (magit-push-arguments))
     (user-error "No branch is checked out")))
  (--when-let (magit-stage-and-commit-with-single-line msg)
    (magit-push-current target args)))

;;;###autoload
(defun magit-stage-and-commit-with-single-line-and-push-fast (msg &rest args)
  "Magit commit amend without editing followed by force push."
  (interactive (read-from-minibuffer "Commit msg: " msg))
  (let ((msg msg)
        (target (magit-get-upstream-branch)))
    (magit-stage-modified t)
    (magit-stage-and-commit-with-single-line-and-push msg
                                                      target)))

;;;###autoload
(defun magit-stage-and-commit-correction-fast (&rest args)
  "Magit commit amend without editing followed by force push."
  (interactive)
  (magit-stage-modified t)
  (magit-stage-and-commit-with-single-line-and-push-fast magit-single-line-fast-commit-msg))


;;;###autoload
(defun magit-commit-amend-noedit (&rest args)
  "Magit commit amend without editing."
  (interactive)
  (magit-commit-amend '("--no-edit")))


;;;###autoload
(defun magit-push-current-force (target &rest args)
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
(defun magit-commit-amend-noedit-push-current-force (target &rest args)
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

;; (transient-append-suffix 'magit-commit
;;   "c" '("F" "Fast commit push" magit-commit-with-single-line-and-push))


(defun get-words-from-command-output-safe (command)
  "Run an external shell COMMAND and return a list of words from its output.
If the command fails, return nil."
  (let ((output (with-output-to-string
                  (with-current-buffer standard-output
                    (call-process-shell-command command nil t)))))
    (if (string-empty-p output)
        nil
      (remove-if-not #'(lambda (s) (> (length s) 0))
                     (split-string output "[[:space:]\n]+")))))

(defun gita-read-group (&optional allow-empty)
  (magit-completing-read "Group"                                              ;prompt
                         (get-words-from-command-output-safe "gita group ls") ;collection
                         nil                                                  ;PREDICATE
                         nil                                                  ;REQUIRE-MATCH
                         nil                                                  ;INITIAL-INPUT
                         nil                                                  ;HIST
                         (when allow-empty "")                                ;DEF
                         nil))                                                ;FALLBACK

(transient-define-infix gita-group ()
  :description "Branch"
  :class 'transient-option
  :key "-b"
  :argument "--branch"
  :reader #'(lambda (prompt initial-input history)
              (read-string "Branch: ")))

(transient-define-infix my-gita-verbose ()
  :description "Verbose"
  :class 'transient-option
  :key "-v"
  :argument "--verbose")

(transient-define-infix my-gita-no-edit ()
  :description "No Edit"
  :class 'transient-option
  :key "--no-edit"
  :argument "--no-edit")


(transient-define-infix my-gita-branch ()
  :description "Branch"
  :class 'transient-option
  :key "-b"
  :argument "--branch"
  :reader #'(lambda (prompt initial-input history) (format "--branch=%s" (read-string "Branch: "))))



;; call-process
;; make-process
;; start-process

;; (defun call-async-process (program &optional infile destination display &rest args)
;;   (make-process :name "gita-push"
;;                 :buffer destination
;;                 :command
;;                 :stderr destination
;;                 :sentinel (lambda (process event)
;;                             (when (string= event "finished\n")
;;                               (with-current-buffer (process-buffer process)
;;                                 (read-only-mode 1)
;;                                 (display-buffer (process-buffer process))))
;;                             (when (string-prefix-p "exited" event)
;;                               (message "Gita push process exited with: %s" event)))))


(defun gita-cmd-display (cmd &rest args)
  "Call the 'gita stat' command and display its output in a new buffer."
  (interactive)
  (let ((output-buffer (get-buffer-create (format "*%s %s*"
                                                  (capitalize cmd)
                                                  (capitalize (car args))))))
    (with-current-buffer output-buffer
      (read-only-mode -1)
      (erase-buffer)
      ;; (display-buffer output-buffer)
      (pop-to-buffer output-buffer)
      (let ((transient-buffer (transient-scope))
            (process (apply #'start-process cmd
                            output-buffer
                            cmd
                            (remove nil args))))
        (set-process-sentinel process
                              #'(lambda (process event)
                                  (when (string-match "finished\\|exited" event)
                                    (let ((exit-code (process-exit-status process)))
                                      (message "Refreshing Magit Buffer %s" transient-buffer)
                                      (when transient-buffer
                                        (with-current-buffer transient-buffer
                                          (when (eq major-mode 'magit-status-mode)
                                            (magit-refresh-buffer))))
                                      (with-current-buffer (process-buffer process)
                                        (read-only-mode 1))
                                      (if (zerop exit-code)
                                          (message "Process finished with exit code: %d" exit-code)
                                        (message "%s %s failed with exit code: %d"
                                                 (capitalize cmd)
                                                 (car args)
                                                 exit-code))))))))))

(defun gita-cmd-execute (cmd &rest args)
  "Call the 'gita status' command and display its output in a new buffer."
  (interactive)
  (let ((output-buffer (get-buffer-create (format "*%s %s*"
                                                  (capitalize cmd)
                                                  (capitalize (car args))))))
    (with-current-buffer output-buffer
      (read-only-mode -1)
      (erase-buffer)
      (display-buffer output-buffer)
      (let ((transient-buffer (transient-scope))
            (process (apply #'start-process cmd
                            output-buffer
                            cmd
                            (remove nil args))))
        (set-process-sentinel process
                              #'(lambda (process event)
                                  (when (string-match "finished\\|exited" event)
                                    (message "Refreshing Magit Buffer %s" transient-buffer)
                                    (when transient-buffer
                                      (with-current-buffer transient-buffer
                                        (when (eq major-mode 'magit-status-mode)
                                          (magit-refresh-buffer))))
                                    (let ((exit-code (process-exit-status process)))
                                      (with-current-buffer (process-buffer process)
                                        (read-only-mode 1))
                                      (if (zerop exit-code)
                                          (message "Process finished with exit code: %d" exit-code)
                                        (message "%s %s failed with exit code: %d"
                                                 (capitalize cmd)
                                                 (car args)
                                                 exit-code))))))))))

(defun gita-stat ()
  "Call the 'gita stat' command and display its output in a new buffer."
  (interactive)
  (gita-cmd-display "gita" "stat" (gita-read-group t)))

(defun gita-status ()
  "Call the 'gita status' command and display its output in a new buffer."
  (interactive)
  (gita-cmd-display "gita" "st" (gita-read-group)))

(defun gita-ssmfor-st ()
  "Call the 'gita status' command and display its output in a new buffer."
  (interactive)
  (gita-cmd-display "gita" "ssmfor-st" (gita-read-group)))

(defun gita-ssmfor-pull-rebase ()
  "Call the 'gita status' command and display its output in a new buffer."
  (interactive)
  (gita-cmd-execute "gita" "ssmfor-pull-rebase" (gita-read-group)))

(defun gita-ssmfor-correct ()
  "Call the 'gita status' command and display its output in a new buffer."
  (interactive)
  (gita-cmd-execute "gita" "ssmfor-correct" (gita-read-group)))

(defun gita-ssmfor-correct-push ()
  "Call the 'gita status' command and display its output in a new buffer."
  (interactive)
  (gita-cmd-execute "gita" "ssmfor-correct-push" (gita-read-group)))


(defun magit-ext-verify ()
  "Run an external PROGRAM, interactively provide input, and handle process timeout."
  (interactive)
  (let ((process (make-process
                  :name "git verify"
                  :buffer "*Git Verify*"
                  :command (list "git" "verify")
                  :sentinel (lambda (proc event)
                              (message "Process %s finished with event: %s"
                                       (process-name proc) event))
                  :filter (lambda (proc output)
                            (with-current-buffer (process-buffer proc)
                              (goto-char (point-max))
                              (insert output))))))
    (with-current-buffer (process-buffer process)
      (erase-buffer)) ;; Clear the buffer for new output
    ;; Tail-recursive loop for process input
    (cl-labels ((process-input-loop ()
                  (if (process-live-p process)
                      (progn
                        ;; Check for user input
                        (let ((input (read-key-sequence-vector "Press any key to input OTP (or wait): "
                                                               nil
                                                               t)))
                          (if (and input (not (equal input "")))
                              (let ((input-str (read-string "OTP: ")))
                                (process-send-string process
                                                     (concat input-str "\n"))
                                (message "Sent input: %s" input-str))
                            (message "Waiting for program completion...")))
                        ;; Continue the loop
                        (process-input-loop))
                    (message "Program completed or timed out."))))
      ;; Start the loop
      (process-input-loop))))

(defun gita-demo (&rest args)
  "Call the 'gita status' command and display its output in a new buffer."
  (interactive (if current-prefix-arg
                   (list (cons "--amend" (gita-transient-arguments)))
                 (list (gita-transient-arguments))))
  (message "Git Demo args %s and Scope %s" args
           (transient-scope)))

(defalias 'gita-push   #'gita-demo)
(defalias 'gita-rebase #'gita-demo)
(defalias 'gita-commit #'gita-demo)
(defalias 'gita-diff #'gita-demo)
(defalias 'gita-reset #'gita-demo)
(defalias 'gita-fetch #'gita-demo)
(defalias 'gita-log #'gita-demo)



(defun magit-extended-action-arguments nil
  (transient-args 'magit-extended-action))

(transient-define-prefix magit-extended-action-menu ()
  "Transient menu for Gita commands."
  :scope (magit-get-mode-buffer 'magit-status-mode) ;; Define scope
  [["Arguments"
    ("-v" "Verbose" "--verbose")
    ("--no-edit" "No Edit" "--no-edit")
    (my-gita-branch)
    (my-gita-verbose)
    (my-gita-no-edit)]]

  [["Git Fast Commands"
    ("cc" "Fast Commit" magit-commit-with-single-line-and-push)
    ("cC" "Fast Commit " magit-commit-with-single-line-and-push-fast)
    ("cf" "Fast Commit Correction" magit-commit-correction-fast)
    ("cA" "Fast Amend"  magit-commit-amend-noedit-push-current-force)
    ("V" "Verify"      magit-ext-verify)]
   ["Gita Status"
    ("ss" "Status" gita-ssmfor-st)
    ("sS" "Status Top" gita-status)
    ("st" "Stat" gita-stat)]

   ["Gita Advanced Commands"
    ("F" "ssmfor-pull-rebase"  gita-ssmfor-pull-rebase)
    ("C" "ssmfor-correct" gita-ssmfor-correct)
    ("P" "ssmfor-correct-push" gita-ssmfor-correct-push)]
   ["Gita Miscellaneous"
    ("d" "Diff" gita-diff)
    ("x" "Reset" gita-reset)]])

(defun magit-extended-action ()
  "Launch the Gita transient menu."
  (interactive)
  (let ((buffer (current-buffer)))
    (transient-setup 'magit-extended-action-menu)))

;;;###autoload
(defun magit-ext-insinuate ()
  (interactive)
  (with-eval-after-load 'magit-mode
    (when t ;; forge-add-default-bindings
      (keymap-set magit-mode-map "C-c C-f" #'magit-extended-action)
      ;; (keymap-set magit-mode-map "N" #'forge-dispatch)
      ;; (keymap-set magit-mode-map "<remap> <magit-browse-thing>"
      ;;             #'forge-browse)
      ;; (keymap-set magit-mode-map "<remap> <magit-copy-thing>"
      ;;             #'forge-copy-url-at-point-as-kill)
      (keymap-set magit-mode-map "C-c C-x f" #'magit-extended-action))))

;;;###autoload
(defun magit-ext-uninsinuate ()
  (interactive)
  (with-eval-after-load 'magit-mode
    (when t ;; forge-add-default-bindings
      (keymap-set magit-mode-map "C-c C-f" nil)
      ;; (keymap-set magit-mode-map "N" #'forge-dispatch)
      ;; (keymap-set magit-mode-map "<remap> <magit-browse-thing>"
      ;;             #'forge-browse)
      ;; (keymap-set magit-mode-map "<remap> <magit-copy-thing>"
      ;;             #'forge-copy-url-at-point-as-kill)
      (keymap-set magit-mode-map "C-c f" nil))))

;;; magit-ext.el ends here

;; (transient-define-argument magit:--gpg-sign ()
;;   :description "Sign using gpg"
;;   :class 'transient-option
;;   :shortarg "-S"
;;   :argument "--gpg-sign="
;;   :allow-empty t
;;   :reader #'magit-read-gpg-signing-key)

;; (defun magit-transient-read-person (prompt initial-input history)
;;   (magit-completing-read
;;    prompt
;;    (mapcar (lambda (line)
;;              (save-excursion
;;                (and (string-match "\\`[\s\t]+[0-9]+\t" line)
;;                     (list (substring line (match-end 0))))))
;;            (magit-git-lines "shortlog" "-n" "-s" "-e" "HEAD"))
;;    nil nil initial-input history))


;; (defun magit-commit-create (&rest args)
;;   "Create a new commit on `HEAD'.
;; With a prefix argument, amend to the commit at `HEAD' instead.
;; \n(git commit [--amend] ARGS)"
;;   (interactive (if current-prefix-arg
;;                    (list (cons "--amend" (magit-commit-arguments)))
;;                  (list (magit-commit-arguments))))
;;   (cond ((member "--all" args)
;;          (setq this-command 'magit-commit--all))
;;         ((member "--allow-empty" args)
;;          (setq this-command 'magit-commit--allow-empty)))
;;   (when (setq args (magit-commit-assert args))
;;     (let ((default-directory (magit-toplevel)))
;;       (magit-run-git-with-editor "commit" args))))

;; (defun magit-commit-arguments nil
;;   (transient-args 'magit-commit))

;; (transient-define-argument magit-commit:--reuse-message ()
;;   :description "Reuse commit message"
;;   :class 'transient-option
;;   :shortarg "-C"
;;   :argument "--reuse-message="
;;   :reader #'magit-read-reuse-message
;;   :history-key 'magit-revision-history)

;; (transient-define-argument magit:--author ()
;;   :description "Limit to author"
;;   :class 'transient-option
;;   :key "-A"
;;   :argument "--author="
;;   :reader #'magit-transient-read-person)






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
