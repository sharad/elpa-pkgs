;;; compile+.el --- compile plus                     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sharad

;; Author: s <>
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

(defcustom compile+-buffer-name " *compile multi cmds*" "compile+ buffer name")
(defcustom compile+-comnit nil "compile+-comnit")
(defcustom compile+-window-setup 'split-window-below "compile+-window-setup")
(defcustom compile+-tmp-file "/tmp/compile" "compile+-tmp-file")


(defvar-local compile+--source-buffer nil)
(put 'compile+--source-buffer 'permanent-local t)

(defvar-local compile+--run nil)
(put 'compile+--run 'permanent-local t)


(defun compile+-edit-abort ()
  (interactive)
  (let ((buffer (get-buffer-create compile+-buffer-name)))
    (with-current-buffer buffer
      (let ((compile+--run nil))
        (compile+-edit-exit)))))
(defun compile+-edit-exit ()
  "Kill current sub-editing buffer and return to source buffer."
  (interactive)
  ;; (set-buffer-modified-p nil)
  (let ((edit-buffer (current-buffer))
	      (source-buffer compile+--source-buffer))
    (if compile+--run
        (progn
          (write-region nil
                        t
                        compile+-tmp-file
                        nil)
          (set-file-modes compile+-tmp-file
			                    (logior (file-modes compile+-tmp-file)
                                  #o100))
          (when compile+--saved-temp-window-config
            (unwind-protect
	              (set-window-configuration compile+--saved-temp-window-config)
	            (setq compile+--saved-temp-window-config nil)))
          (org-src-switch-to-buffer source-buffer 'exit)
          (compile (format "bash -c %s"
                           compile+-tmp-file)
                   compile+-comnit))
      (org-src-switch-to-buffer source-buffer 'exit))))

(defvar compile+-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c'" 'compile+-edit-exit)
    (define-key map "\C-c\C-c" 'compile+-edit-exit)
    (define-key map "\C-c\C-k" 'compile+-edit-abort)
    (define-key map "\C-x\C-s" 'compile+-edit-save)
    map))

(defun compile+-hook (process)
  (let ((buff (process-buffer process)))
    (with-current-buffer buff
      (let ((buffer-read-only nil)
            (content (with-temp-buffer
                       (insert-file-contents compile+-tmp-file)
                       (buffer-string))))
        (goto-line 1)
        (replace-string (format "bash -c %s"
                                compile+-tmp-file)
                        (concat "*Running commands\n"
                                content
                                "\n*End of commands\n"))))))

(define-minor-mode compile+-mode "compile+ mode" nil " compile+"
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c'" 'compile+-edit-exit)
    (define-key map "\C-c\C-c" 'compile+-edit-exit)
    (define-key map "\C-c\C-k" 'compile+-edit-abort)
    (define-key map "\C-x\C-s" 'compile+-edit-save)
    map)
  (if compile+-mode
      (message "enabled compile+-mode %s" compile+-mode)
    (message "disabled compile+-mode %s" compile+-mode)))


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

(defun compile+--edit (name source-buffer &optional initialize run)
  (when (memq compile+-window-setup '(reorganize-frame
				                             split-window-below
				                             split-window-right))
    (setq compile+--saved-temp-window-config
          (current-window-configuration)))
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (when (functionp initialize)
        (funcall initialize))
      (compile+-mode)
      (use-local-map (copy-keymap compile+-mode-map))
      (local-set-key "\C-c\C-c" 'compile+-edit-exit)
      (setq compile+--source-buffer source-buffer)
      (setq compile+--run run))
    (compile+-switch-to-buffer buffer 'edit)))


;;;###autoload
(defun compile+-edit-block ()
  (interactive)
  (let ((mode #'shell-script-mode)
        (source-buffer (current-buffer)))
    (unless (functionp mode)
      (error "No such language mode: %s" mode))
    (compile+--edit compile+-buffer-name source-buffer mode t)))

(defalias 'compile+ #'compile+-edit-block)


;;;###autoload
(defun compile+-insinuate ()
  (interactive)
  (define-key c-mode-base-map [remap compile] 'compile+)
  (add-hook 'compilation-start-hook #'compile+-hook))
;;;###autoload
(defun compile+-uninsinuate ()
  (interactive)
  (define-key c-mode-base-map [remap compile] nil)
  (remove-hook 'compilation-start-hook #'compile+-hook))

;;; compile+.el ends here
