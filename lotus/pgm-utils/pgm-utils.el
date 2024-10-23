;;; pgm-utils.el --- pgm mode utils                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  sharad

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

(provide 'pgm-utils)


;; (autoload 'magit-git-lines "magit")
;; (autoload 'magit-process-file "magit")

(require 'magit)

;; TODO: UNFINISHED

;;;###autoload
(defvar office-git-remote-regex "")


;;;###autoload
(define-minor-mode office-mode
  "Prepare for working with collarative office project. This
is the mode to be enabled when I am working in some files on
which other peoples are also working."
  :initial-value nil
  :lighter " Office"
  :global nil
  (condition-case e
      (if office-mode
          (progn
            (message "calling enable office mode")
            (when (or
                   (eq major-mode 'c-mode)
                   (eq major-mode 'c++-mode))
              (setq tab-width 8)
              (c-set-style "stroustrup" 1))
            (setq show-trailing-whitespace nil)
            (set (make-local-variable 'before-save-hook) before-save-hook)
            (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
            (remove-hook 'write-file-hooks 'delete-trailing-whitespace t)
            (run-with-timer
             7 nil
             #'(lambda (buff)
                 (when (and (bufferp buff)
                            (buffer-live-p buff))
                   (with-current-buffer buff (forgive/them))))
             (current-buffer))
            (message "called enable office mode"))

        (progn
          (message "calling disable office mode")
          (when (or (eq major-mode 'c-mode)
                    (eq major-mode 'c++-mode))
            (setq tab-width (custom-reevaluate-setting 'tab-width))
            (c-set-style "gnu" 1))
          (setq show-trailing-whitespace 1)
          (set (make-local-variable 'before-save-hook) before-save-hook)
          (add-hook 'before-save-hook 'delete-trailing-whitespace t)
          (add-hook 'write-file-hook 'delete-trailing-whitespace t)
          (message "called disable office mode")))
    (error (message "Error: %s" e))))

;;;###autoload
(defun office-file-p (file)
  (let ((remote-repo (cl-first (remove-if-not #'(lambda (s)
                                               (when s
                                                 (string-match-p "^origin" s)))
                                           (magit-git-lines "remote" "-v")))))
    (when (and office-git-remote-regex
               (functionp 'magit-git-lines)
               remote-repo)
      (string-match-p office-git-remote-regex
                      remote-repo))))

;;;###autoload
(defun office-activate ()
  (interactive)
  (let ((file (buffer-file-name)))
    (when (and file
               (office-file-p file))
      ;; if file is handled by perforce than assume it is
      ;; related to office perforce repository.
      (office-mode 1))))

;;;###autoload
(defun office-init ()
  (add-hook 'prog-mode-hook 'office-activate)
  (add-hook 'nxml-mode-hook 'office-activate))


;; TODO: UNFINISHED
(defvar phased-features nil)

(defun phased-feature-add (condition feature))
(defun phased-feature-remove (condition feature))

(defun phased-feature-disable ()
  ())

(defun phased-feature-enable ()
  ())

(defun phased-feature ()
  (let ((curr (current-buffer)))
    (if (eq curr (window-buffer (selected-window)))
        ()
      ())))

;;; pgm-utils.el ends here
