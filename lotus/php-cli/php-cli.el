;;; php-cli.el --- php cli                            -*- lexical-binding: t; -*-

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

(provide 'php-cli)


(require 'ac-php-core)


(defvar php-cli-file-path "php"
  "Path to the program used by `run-php-cli'")

(defvar php-cli-arguments '("-a")
  "Commandline arguments to pass to `php-cli'.")

(defvar php-cli-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    ;; (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-php-cli'.")

(defvar php-cli-prompt-regexp "^\\(?:php >\\)" ;; "^\\(?:\\[[^@]+@[^@]+\\]\\)"
  "Prompt for `run-php-cli'.")


(defvar php-cli-buffer-name "*Php-Cli*"
  "Name of the buffer to use for the `run-php-cli' comint instance.")

(defun run-php-cli ()
  "Run an inferior instance of `php-cli' inside Emacs."
  (interactive)
  (let* ((php-cli-program php-cli-file-path)
         (buffer (get-buffer-create php-cli-buffer-name))
         (proc-alive (comint-check-proc buffer))
         (process (get-buffer-process buffer)))
    ;; if the process is dead then re-create the process and reset the
    ;; mode.
    (unless proc-alive
      (with-current-buffer buffer
        (apply 'make-comint-in-buffer "Php-Cli" buffer
               php-cli-program nil php-cli-arguments)
        (php-cli-mode)))
    ;; Regardless, provided we have a valid buffer, we pop to it.
    (when buffer
      (pop-to-buffer buffer))))


(defun php-cli--initialize ()
  "Helper function to initialize Php-Cli."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode php-cli-mode comint-mode "Php-Cli"
  "Major mode for `run-php-cli'.

\\<php-cli-mode-map>"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp php-cli-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(php-cli-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) php-cli-prompt-regexp))

(add-hook 'php-cli-mode-hook 'php-cli--initialize)

(defconst php-cli-keywords ac-php--php-key-list
  "List of keywords to highlight in `php-cli-font-lock-keywords'.")

(defvar php-cli-font-lock-keywords
  (list
   ;; highlight all the reserved commands.
   `(,(concat "\\_<" (regexp-opt ac-php--php-key-list) "\\_>") . font-lock-keyword-face))
  "Additional expressions to highlight in `php-cli-mode'.")

;;; php-cli.el ends here
