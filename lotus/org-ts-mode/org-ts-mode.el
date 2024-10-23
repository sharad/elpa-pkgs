;;; org-ts-mode.el --- org timestamp mode            -*- lexical-binding: t; -*-

;; Copyright (C) 2023  s

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

(provide 'org-ts-mode)


(defface org-ts-mode-lighter
  '((t (:inherit mode-line :foreground "Green")))
  "Face used for lighter tasks in mode line."
  :group 'org-ts)

(defvar org-ts-mode-main-keymap (make-keymap) "occ-mode keymap.")
(define-prefix-command 'org-ts-mode-keymap)
;;;###autoload
(defun occ-enable-mode-map ()
  (define-key org-ts-mode-main-keymap (kbd occ-prefix-key) 'org-ts-mode-keymap))
;;;###autoload
(defun occ-disable-mode-map ()
  (define-key org-ts-mode-main-keymap (kbd occ-prefix-key) nil))

(let ((last-insertion (current-time)))
  ;; (org-newline-and-indent)
  ;; (org--newline)
  ;; (org-return)

  ;; (defun org--newline (indent arg interactive)
  ;;   "Call `newline-and-indent' or just `newline'.
  ;; If INDENT is non-nil, call `newline-and-indent' with ARG to
  ;; indent unconditionally; otherwise, call `newline' with ARG and
  ;; INTERACTIVE, which can trigger indentation if
  ;; `electric-indent-mode' is enabled."
  ;;   (if indent
  ;;       (org-newline-and-indent arg)
  ;;     (newline arg interactive)))

  (defun org-ts-mode-around--org--newline (oldfn &rest args)
    (let ((current-time (current-time)))
      (when (< 10
               (- (float-time current-time)
                  (float-time last-insertion)))
        (org-insert-time-stamp nil nil nil " ")
        (setq last-insertion current-time)))
    (apply oldfn args)))

;;;###autoload
(autoload 'org-ts-mode-around--org--newline "lotus-misc-advices" "\


\(fn OLDFUN &rest ARGS)" nil nil)

;;;###autoload
(define-minor-mode org-ts-mode
  "Toggle Org timestamp mode.
      ...rest of documentation as before..."
  ;; The initial value.
  :init-value nil
  :global     nil
  ;; The indicator for the mode line.
  :lighter (:eval (propertize " ots" 'face 'org-ts-mode-lighter))
  ;; The minor mode bindings.
  :keymap org-ts-mode-main-keymap
  :group 'occ
  (if org-ts-mode
      (with-eval-after-load "org"
        (add-function :around
                      (symbol-function 'org--newline)
                      #'org-ts-mode-around--org--newline))
    (remove-function (symbol-function 'org--newline)
                     #'org-ts-mode-around--org--newline)))

;;; org-ts-mode.el ends here
