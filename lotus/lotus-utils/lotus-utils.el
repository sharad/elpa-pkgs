;;; lotus-utils.el --- copy config  -*- lexical-binding: t; -*-

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; will not require any library, only will provide utils
;; assume file from here are always available.

(provide 'lotus-utils)


(require 'helm-core)
;; (require 'elscreen)
(require 'vc)
(require 'lotus-misc-utils)


;;;###autoload
(defun vc-checkout-file (file)
  (condition-case e
      (let ((default-directory (file-name-directory file)))
        (vc-checkout file)
        t)
    ('file-error (message "error: %s" e) nil)))

;;;###autoload
(defun touch-file (file)
  (interactive)
  ;; https://stackoverflow.com/questions/2592095/how-do-i-create-an-empty-file-in-emacs/2592558#2592558
  (unless (file-exists-p file)
    (make-directory (file-name-directory file) t)
    (with-temp-buffer
      (write-file file)))
  file)

;;;###autoload
(defun cleanup-tty-process ()
  (interactive)
  (let ((tty-processes (cl-remove-if-not 'process-tty-name
                                         (process-list))))
    (dolist (tp tty-processes)
      (kill-process tp))))

;;;###autoload
(defun elscreen-keymap-setup ()
  (progn
    (progn
      (turn-off-evil-mode)
      ;; https://github.com/syl20bnr/spacemacs/issues/7372
      (define-key evil-emacs-state-map (kbd "C-z") nil)
      (global-unset-key [C-z]))

    (when (featurep 'centaur-tabs)
      ;; "Keybinding: Centaur-Tabs"
      ;; (global-set-key [C-z c] 'centaur-tabs-create)
      (funcall #'(lambda (symbol value)
                   (when (fboundp 'centaur-tabs-set-prefix-key)
                     (centaur-tabs-set-prefix-key value))
                   (custom-set-default symbol value))
               'centaur-tabs-prefix-key "\s-z")
      (global-set-key-if-unbind [C-H-right] 'centaur-tabs-forward)
      (global-set-key-if-unbind [C-H-left]  'centaur-tabs-backward)
      (global-set-key-if-unbind [M-H-right] 'centaur-tabs-forward-group)
      (global-set-key-if-unbind [M-H-left]  'centaur-tabs-backward-group)
      (global-set-key-if-unbind [M-H-up] 'centaur-tabs-swap))

    (when (featurep 'tab-bar)
      (defvar lotus-tabs-bar-prefix-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "C-a") 'tab-bar-switch-to-prev-tab)
          (define-key map (kbd "C-c") 'tab-bar-new-tab)
          (define-key map (kbd "c") 'tab-bar-new-tab)
          ;; (define-key map (kbd "C-f") 'elscreen-find-file)
          (define-key map (kbd "C-k") 'tab-bar-close-tab)
          (define-key map (kbd "k") 'tab-bar-close-tab)
          (define-key map (kbd "RET") 'elscreen-display-last-message)
          (define-key map (kbd "C-n") 'tab-bar-switch-to-next-tab)
          (define-key map (kbd "C-p") 'tab-bar-switch-to-prev-tab)
          (define-key map (kbd "n") 'tab-bar-switch-to-next-tab)
          (define-key map (kbd "p") 'tab-bar-switch-to-prev-tab)
          map))
      (defvar tab-bar-prefix-key (kbd "C-z"))
      ;; (define-key tab-bar-map tab-bar-prefix-key lotus-tabs-bar-prefix-map)
      (global-set-key tab-bar-prefix-key lotus-tabs-bar-prefix-map)

      ;; (spacemacs/set-leader-keys
      ;;   "C-r" lotus-xref-command-map)

      (global-set-key-if-unbind [s-right] 'tab-bar-switch-to-next-tab)
      (global-set-key-if-unbind [s-left]  'tab-bar-switch-to-prev-tab)
      (global-set-key-if-unbind [s-up]    'tab-bar-switch-to-recent-tab)
      (global-set-key-if-unbind [s-down]  'tab-bar-switch-to-last-tab)
      (global-set-key-if-unbind [H-right] 'tab-bar-move-tab)
      (global-set-key-if-unbind [H-left]  'tab-bar-move-tab-backward)
      (global-set-key-if-unbind [H-down]  'tab-bar-new-tab)
      (global-set-key-if-unbind [H-up]    'tab-bar-close-tab))))

;;;###autoload
(defun resolveip (host)
  (= 0
     (call-process "~/bin/resolveip" nil nil nil host)))

;;;###autoload
(defun host-accessable-p (&optional host)
  (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
                     (if host host "www.google.com"))))

(progn                                  ;debug testing code
  (defvar *test-idle-prints-timer* nil)

  ;;;###autoload
  (defun test-idle-prints (print)
    (if print
        (progn
          (defvar known-last-input-event nil)
          (if *test-idle-prints-timer* (cancel-timer *test-idle-prints-timer*))
          (when t
            (setq *test-idle-prints-timer*
                  (run-with-timer 1 2
                                  #'(lambda ()
                                      ;; (message "Test: From timer idle for org %d secs emacs %d secs" (org-emacs-idle-seconds) (float-time (current-idle-time)))
                                      (let* (display-last-input-event
                                             (idle (current-idle-time))
                                             (idle (if idle (float-time (current-idle-time)) 0)))
                                        (unless (eq known-last-input-event last-input-event)
                                          (setq display-last-input-event last-input-event
                                                known-last-input-event last-input-event))
                                        (message "Test: From timer idle for %f secs emacs, and last even is %s" idle display-last-input-event)))))))
      (when *test-idle-prints-timer*
        (cancel-timer *test-idle-prints-timer*))))

  ;;;###autoload
  (defun toggle-test-idle-prints ()
    (interactive)
    (test-idle-prints (null *test-idle-prints-timer*)))

  ;;;###autoload
  (defun lotus-necessary-test ()
    (interactive)
    (test-idle-prints nil)))

;;;###autoload
(defun reset-helm-input ()
  (interactive)
  ;; https://github.com/emacs-helm/helm/issues/208#issuecomment-14447049
  ;; https://www.emacswiki.org/emacs/RecursiveEdit
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Recursive-Editing.html#Recursive-Editing
  ;; http://ergoemacs.org/emacs/elisp_break_loop.html
  (message "recursion-depth %d" (recursion-depth))
  (message "helm-alive-p %s"    helm-alive-p)
  (get-buffer-create "*helm*")
  (setq helm-alive-p nil)
  (safe-exit-recursive-edit-if-active)
  (safe-exit-recursive-edit))

;; (global-set-key-warn-if-bind (kbd "C-H-k") 'reset-helm-input)
;; (global-set-key-warn-if-bind (kbd "C-H-q") 'reset-helm-input)
;; (global-set-key-warn-if-bind (kbd "C-H-g") 'reset-helm-input)
;; (global-set-key-warn-if-bind (kbd "C-H-h") 'reset-helm-input)

(require 's)
(require 'dash)
(defun s-lcp (&rest strings)
  (if (null strings)
      ""
    (-reduce-from #'s-shared-start (cl-first strings) (cl-rest strings))))


;;;###autoload
(defun forgive/them ()
  (interactive)
  (if (and (featurep 'develock)
           (require 'develock)
           (assq major-mode develock-keywords-alist))
      (develock-mode -1))
  (highlight-changes-visible-mode -1))


(defun run-process (name buffer success-fn program &rest program-args)
  "run a program in a subprocess with persistant buffer. Return the
process object for it.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer name) to associate with the process.

Process output (both standard output and standard error streams)
goes at end of BUFFER, unless you specify a filter function to
handle the output.  BUFFER may also be nil, meaning that this
process is not associated with any buffer.

SUCCESS-FN if non NIL then run it when program is successful.

PROGRAM is the program file name.  It is searched for in `exec-path'
\(which see).  If nil, just associate a pty with the buffer.  Remaining
arguments PROGRAM-ARGS are strings to give program as arguments.

If you want to separate standard output from standard error, use
`make-process' or invoke the command through a shell and redirect
one of them using the shell syntax.

The process runs in `default-directory' if that is local (as
determined by `unhandled-file-name-directory'), or \"~\"
otherwise.  If you want to run a process in a remote directory
use `start-file-process'."
  (unless (fboundp 'make-process)
    (error "Emacs was compiled without subprocess support"))
  (let* ((buffer (get-buffer buffer))
         (start-marker (when buffer
                         (with-current-buffer buffer
                           (buffer-end 1)))))
    (make-process :name name
                  :buffer buffer
                  :command (if program
                               (list :command (cons program program-args)))
                  :sentinel #'(lambda (process event)
                                "Sentinel to handle process exit status."
                                (when (string-match-p "finished\\|exited" event)
                                  (let ((exit-code (process-exit-status process)))
                                    (if (zerop exit-code)
                                        (when success-fn
                                          (funcall success-fn))
                                      (let* ((end-marker (if buffer
                                                             (with-current-buffer buffer
                                                               (buffer-end 1))))
                                             (text (with-current-buffer buffer
                                                     (if (and start-marker
                                                              end-marker)
                                                         (buffer-substring start-marker
                                                                           end-marker)
                                                       (when end-marker (buffer-string))))))
                                        (error "Process %s failed with exit code %d\n%s"
                                               (process-name process)
                                               exit-code
                                               text)))))))))

;;; lotus-utils.el ends here
