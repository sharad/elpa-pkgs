;;; occ-mode.el --- occ mode                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d@gmail.com>
;; Keywords: convenience, tools

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

(provide 'occ-mode)


(require 'occ-config)


(defvar occ-mode-main-keymap (make-keymap) "occ-mode keymap.")

;; https://www.emacswiki.org/emacs/PrefixKey
;; https://stackoverflow.com/questions/25524710/define-key-in-prefix-keymap-for-a-particular-mode

(define-prefix-command 'occ-mode-keymap)

(defun occ-enable-mode-map ()
  (define-key occ-mode-main-keymap (kbd occ-prefix-key) 'occ-mode-keymap))

(defun occ-disable-mode-map ()
  (define-key occ-mode-main-keymap (kbd occ-prefix-key) nil))


(define-key occ-mode-keymap (kbd "cr") 'occ-run)
(define-key occ-mode-keymap (kbd "ms") 'occ-helm-match-select)
(define-key occ-mode-keymap (kbd "ls") 'occ-helm-list-select)
(define-key occ-mode-keymap (kbd "ld") 'occ-helm-list-debug-select)
(define-key occ-mode-keymap (kbd "ll") 'occ-helm-list-launch)
(define-key occ-mode-keymap (kbd "le") 'occ-property-edit)


(define-key occ-mode-keymap (kbd "Cc") 'occ-curr-procreate-child)
(define-key occ-mode-keymap (kbd "CC") 'occ-curr-procreate-child-clock-in)
(define-key occ-mode-keymap (kbd "Ct") 'occ-curr-tsk-continue-for)

(define-key occ-mode-keymap (kbd "ds") 'occ-start-day)
(define-key occ-mode-keymap (kbd "du") 'occ-show-up)
(define-key occ-mode-keymap (kbd "do") 'occ-stop-day)
(define-key occ-mode-keymap (kbd "dp") 'occ-pack-up)

(define-key occ-mode-keymap (kbd "tif") 'occ-do-clock-in-force)
(define-key occ-mode-keymap (kbd "tii") 'occ-interrupt-clock-in)
(define-key occ-mode-keymap (kbd "to")  'occ-clock-out)
(define-key occ-mode-keymap (kbd "tP")  'occ-continue-prev)
(define-key occ-mode-keymap (kbd "ta")  'occ-obj-make-anonymous)

(define-key occ-mode-keymap (kbd "q") 'occ-keep-quiet)
(define-key occ-mode-keymap (kbd "Q") 'occ-keep-quiet-for)

(define-key occ-mode-keymap (kbd "rr") 'occ-register-resolve-clock)
(define-key occ-mode-keymap (kbd "ru") 'occ-unregister-resolve-clock)

(define-key occ-mode-keymap (kbd "sR") 'occ-reset-spec)
(define-key occ-mode-keymap (kbd "sM") 'occ-obj-make-spec)
(define-key occ-mode-keymap (kbd "sA") 'occ-add-to-spec)
(define-key occ-mode-keymap (kbd "sO") 'occ-add-org-file)
(define-key occ-mode-keymap (kbd "sB") 'occ-obj-build-spec)

(define-key occ-mode-keymap (kbd "Tr") 'occ-files-with-null-regex)
(define-key occ-mode-keymap (kbd "To") 'occ-files-not-in-org-mode)

(define-key occ-mode-keymap (kbd "Um") 'occ-merge-unamed-task)

(define-key occ-mode-keymap (kbd "OR") 'occ-reset-collection-object)
(define-key occ-mode-keymap (kbd "Oi") 'occ-insinuate)
(define-key occ-mode-keymap (kbd "Ou") 'occ-uninsinuate)

(define-key occ-mode-keymap (kbd "oT") 'occ-run-timer)
(define-key occ-mode-keymap (kbd "oR") 'occ-reload)
(define-key occ-mode-keymap (kbd "v")  'occ-version)


;;;###autoload
(define-minor-mode occ-mode
  "Toggle Occ mode.
      ...rest of documentation as before..."
  ;; The initial value.
  :init-value nil
  :global     t
  ;; The indicator for the mode line.
  :lighter " Occ"
  ;; The minor mode bindings.
  :keymap occ-mode-main-keymap
  :group 'occ
  (if occ-mode
      (occ-insinuate)
    (occ-uninsinuate)))

;;; occ-mode.el ends here
