;;; session--unified-tab-bar.el --- tab bar unified           -*- lexical-binding: t; -*-

;; Copyright (C) 2024  s

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

(provide 'sessions-unified-tab-bar)


(defvar tab-bar-tabs-function #'tab-bar-tabs
  "Function to get a list of tabs to display in the tab bar.
This function should have one optional argument FRAME,
defaulting to the selected frame when nil.
It should return a list of alists with parameters
that include at least the element (name . TAB-NAME).
For example, \\='((tab (name . \"Tab 1\")) (current-tab (name . \"Tab 2\")))
By default, use function `tab-bar-tabs'.")

(defun tab-bar-tabs (&optional frame)
  "Return a list of tabs belonging to the FRAME.
Ensure the frame parameter `tabs' is pre-populated.
Update the current tab name when it exists.
Return its existing value or a new value."
  (let ((tabs (frame-parameter frame 'tabs)))
    (if tabs
        (let* ((current-tab (tab-bar--current-tab-find tabs))
               (current-tab-name (assq 'name current-tab))
               (current-tab-explicit-name (assq 'explicit-name current-tab)))
          (when (and current-tab-name
                     current-tab-explicit-name
                     (not (cdr current-tab-explicit-name)))
            (setf (cdr current-tab-name)
                  (funcall tab-bar-tab-name-function))))
      ;; Create default tabs
      (setq tabs (list (tab-bar--current-tab-make)))
      (tab-bar-tabs-set tabs frame))
    tabs))

(defun tab-bar-tabs-set (tabs &optional frame)
  "Set a list of TABS on the FRAME."
  (set-frame-parameter frame 'tabs tabs))



(defun tab-bar-frame-data-get (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (let (fsession-data)
      (push (cons 'screens (lotus-elscreen-get-screen-to-name-alist)) fsession-data)
      (push (cons 'current-buffer-file (cons (buffer-name (current-buffer)) (buffer-file-name))) fsession-data)
      (push (cons 'current-screen (elscreen-get-current-screen)) fsession-data)
      (push (cons 'desktop-buffers (lotus-elscreen-get-desktop-buffer-args-list)) fsession-data))))
(defun tab-bar-frame-data-set (fsession-data &optional frame)
  (if fsession-data                    ;may causing error
      (with-selected-frame (or frame (selected-frame)))))

(eval-after-load "tab-bar"
  (sessions-unified-fsession-register-fns 'tab-bar
                                          #'tab-bar-frame-data-get
                                          #'tab-bar-frame-data-set))

;;; sessions-unified-tab-bar.el ends here
