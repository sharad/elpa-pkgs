;;; gita.el --- gita transient                       -*- lexical-binding: t; -*-

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

(provide 'gita)


(eval-when-compile
  (require 'transient))


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
                                          ;; (magit-mode-bury-buffer)
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

;;; gita.el ends here
