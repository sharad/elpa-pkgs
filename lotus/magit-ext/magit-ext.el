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


;; (require 'transient)
;; (require 'magit-mode)
;; (require 'magit-apply)
;; (require 'magit-git)


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
  ;; (interactive)
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
  ;; (interactive)
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


;;;###autoload
(defun gita-stat ()
  "Call the 'gita stat' command and display its output in a new buffer."
  (interactive)
  (gita-cmd-display "gita" "stat" (gita-read-group t)))
;;;###autoload
(defun gita-status ()
  "Call the 'gita status' command and display its output in a new buffer."
  (interactive)
  (gita-cmd-display "gita" "st" (gita-read-group)))
;;;###autoload
(defun gita-ssmfor-st ()
  "Call the 'gita status' command and display its output in a new buffer."
  (interactive)
  (gita-cmd-display "gita" "ssmfor-st" (gita-read-group)))
;;;###autoload
(defun gita-ssmfor-pull-rebase ()
  "Call the 'gita status' command and display its output in a new buffer."
  (interactive)
  (gita-cmd-execute "gita" "ssmfor-pull-rebase" (gita-read-group)))
;;;###autoload
(defun gita-ssmfor-correct ()
  "Call the 'gita status' command and display its output in a new buffer."
  (interactive)
  (gita-cmd-execute "gita" "ssmfor-correct" (gita-read-group)))
;;;###autoload
(defun gita-ssmfor-correct-push ()
  "Call the 'gita status' command and display its output in a new buffer."
  (interactive)
  (gita-cmd-execute "gita" "ssmfor-correct-push" (gita-read-group)))


;;;###autoload
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

(defun magit-ext-insinuate--action-menu ()
  (transient-define-prefix magit-extended-action-menu ()
    "Transient menu for Gita commands."
    :scope (when (magit-toplevel)
             (magit-get-mode-buffer 'magit-status-mode)) ;; Define scope
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
      ("x" "Reset" gita-reset)]]))

;;;###autoload
(defun magit-extended-action ()
  "Launch the Gita transient menu."
  (interactive)
  (let ((buffer (current-buffer)))
    (transient-setup 'magit-extended-action-menu)))

;;;###autoload
(defun magit-ext-insinuate ()
  (interactive)
  (with-eval-after-load "magit-mode"
    (magit-ext-insinuate--action-menu)
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
  (with-eval-after-load "magit-mode"
    (when t ;; forge-add-default-bindings
      (keymap-set magit-mode-map "C-c C-f" nil)
      ;; (keymap-set magit-mode-map "N" #'forge-dispatch)
      ;; (keymap-set magit-mode-map "<remap> <magit-browse-thing>"
      ;;             #'forge-browse)
      ;; (keymap-set magit-mode-map "<remap> <magit-copy-thing>"
      ;;             #'forge-copy-url-at-point-as-kill)
      (keymap-set magit-mode-map "C-c f" nil))))

;;; magit-ext.el ends here
