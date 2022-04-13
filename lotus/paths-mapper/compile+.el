;;; compile+.el --- compile plus                     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sharad

;; Author: sharad <spratap@merunetworks.com>
;; Keywords: convenience, languages, tools

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

;; check https://stackoverflow.com/questions/16992726/how-to-prompt-the-user-for-a-block-of-text-in-elisp
;; https://stackoverflow.com/questions/5034839/emacs-pop-up-bottom-window-for-temporary-buffers
;; https://www.reddit.com/r/emacs/comments/dg9m2f/popup_with_text_input/

;;; Code:

(provide 'compile+)

(defcustom compile+-comnit nil "compile+-comnit")
(defcustom compile+-window-setup 'split-window-below "compile+-window-setup")
(defcustom compile+-tmp-file "/tmp/compile" "compile+-tmp-file")

(defun compile+-edit-abort ())
(defun compile+-edit-exit ()
  "Kill current sub-editing buffer and return to source buffer."
  (interactive)
  (write-region (format "cat %s; echo;\n" compile+-tmp-file)
                t
                compile+-tmp-file
                nil)
  (write-region nil
                t
                compile+-tmp-file
                t)
  (set-file-modes compile+-tmp-file
			            (logior (file-modes compile+-tmp-file)
                          #o100))
  (when compile+--saved-temp-window-config
    (unwind-protect
	      (set-window-configuration compile+--saved-temp-window-config)
	    (setq compile+--saved-temp-window-config nil)))
  (compile (format "bash -c %s"
                   compile+-tmp-file)
           compile+-comnit))
(defvar compile+-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c'" 'compile+-edit-exit)o
    (define-key map "\C-c\C-c" 'compile+-edit-exit)
    (define-key map "\C-c\C-k" 'compile+-edit-abort)
    (define-key map "\C-x\C-s" 'compile+-edit-save)
    map))

(define-minor-mode compile+-mode
  nil " compile+" nil
  )


(defun compile-switch-to-buffer-other-window (&rest args)
  (org-no-popups
   (apply #'switch-to-buffer-other-window args)))

(defun compile+-switch-to-buffer (buffer context)
  (pcase compile+-window-setup
    (`plain
     (when (eq context 'exit) (quit-restore-window))
     (pop-to-buffer buffer))
    (`current-window (pop-to-buffer-same-window buffer))
    (`other-window
     (let ((cur-win (selected-window)))
       (compile-switch-to-buffer-other-window buffer)
       (when (eq context 'exit) (quit-restore-window cur-win))))
    (`split-window-below
     (if (eq context 'exit)
	       (delete-window)
       (select-window (split-window-vertically)))
     (pop-to-buffer-same-window buffer))
    (`split-window-right
     (if (eq context 'exit)
	       (delete-window)
       (select-window (split-window-horizontally)))
     (pop-to-buffer-same-window buffer))
    (`other-frame
     (pcase context
       (`exit
	      (let ((frame (selected-frame)))
	        (switch-to-buffer-other-frame buffer)
	        (delete-frame frame)))
       (`save
	      (kill-buffer (current-buffer))
	      (pop-to-buffer-same-window buffer))
       (_ (switch-to-buffer-other-frame buffer))))
    (`reorganize-frame
     (when (eq context 'edit) (delete-other-windows))
     (compile-switch-to-buffer-other-window buffer)
     (when (eq context 'exit) (delete-other-windows)))
    (`switch-invisibly (set-buffer buffer))
    (_
     (message "Invalid value %s for `compile+-window-setup'"
	            compile+-window-setup)
     (pop-to-buffer-same-window buffer))))

(defun compile+--edit (name &optional initialize)
  (when (memq compile+-window-setup '(reorganize-frame
				                             split-window-below
				                             split-window-right))
    (setq compile+--saved-temp-window-config (current-window-configuration)))
  (let ((buffer (get-buffer-create " *compile multi cmds*")))
    (with-current-buffer buffer
      (compile+-switch-to-buffer buffer 'edit)
      (when (functionp initialize)
        (funcall initialize))
      (compile+-mode)
      (use-local-map (copy-keymap compile+-mode-map))
      (local-set-key "\C-c\C-c" 'compile+-edit-exit))))


;;;###autoload
(defun compile+-edit-block ()
  (interactive)
  (let ((mode #'shell-script-mode))
    (unless (functionp mode)
      (error "No such language mode: %s" mode))
    (compile+--edit "*compile*" mode)))

(defalias 'compile+ #'compile+-edit-block)

;;;###autoload
(define-key c-mode-base-map [remap compile] 'compile+)

;;; compile+.el ends here
