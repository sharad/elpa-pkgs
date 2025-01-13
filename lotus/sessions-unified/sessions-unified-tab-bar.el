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


(require 'sessions-unified-core-common)


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


;; (tab-bar--tab)

;; (tab-bar-tabs)



(defun tab-bar--tab (&optional frame)
  "Make a new tab data structure that can be added to tabs on the FRAME."
  (let* ((tab (tab-bar--current-tab-find nil frame))
         (tab-explicit-name (alist-get 'explicit-name tab))
         (tab-group (alist-get 'group tab))
         (bl  (seq-filter #'buffer-live-p (frame-parameter
                                           frame 'buffer-list)))
         (bbl (seq-filter #'buffer-live-p (frame-parameter
                                           frame 'buried-buffer-list))))
    `(tab
      (name . ,(if tab-explicit-name
                   (alist-get 'name tab)
                 (funcall tab-bar-tab-name-function)))
      (explicit-name . ,tab-explicit-name)
      ,@(if tab-group `((group . ,tab-group)))
      (time . ,(float-time))
      (ws . ,(window-state-get
              (frame-root-window (or frame (selected-frame))) 'writable))
      (wc . ,(current-window-configuration))
      (wc-point . ,(point-marker))
      (wc-bl . ,bl)
      (wc-bbl . ,bbl)
      ,@(when tab-bar-history-mode
          `((wc-history-back . ,(gethash (or frame (selected-frame))
                                         tab-bar-history-back))
            (wc-history-forward . ,(gethash (or frame (selected-frame))
                                            tab-bar-history-forward))))
      ;; Copy other possible parameters
      ,@(mapcan (lambda (param)
                  (unless (memq (car param)
                                '(name explicit-name group time
                                       ws wc wc-point wc-bl wc-bbl
                                       wc-history-back wc-history-forward))
                    (list param)))
                (cdr tab)))))



(defun my-serialize-window-configuration ()
  "Serialize the current window configuration as an S-expression."
  (let ((windows nil))
    (dolist (win (window-list))
      (let ((buffer (buffer-name (window-buffer win)))
            (point (window-point win))
            (start (window-start win))
            (parameters (window-parameters win)))
        (push (list buffer point start parameters) windows)))
    (nreverse windows)))


;; (current-window-configuration)
(window-configuration-get (current-window-configuration) 'windows)




(defun tab-bar-frame-data-get (&optional frame)
  (with-selected-frame (or frame (selected-frame))))

(defun tab-bar-frame-data-set (fsession-data &optional frame)
  (if fsession-data                    ;may causing error
      (with-selected-frame (or frame (selected-frame)))))

;; (eval-after-load "tab-bar"
;;   (sessions-unified-fsession-register-fns 'tab-bar
;;                                           #'tab-bar-frame-data-get
;;                                           #'tab-bar-frame-data-set))

;;; sessions-unified-tab-bar.el ends here
