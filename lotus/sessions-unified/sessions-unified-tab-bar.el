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


(defmacro with-selected-tab (index frame &rest body)
  `(with-selected-frame (or ,frame (selected-frame))
     (let* ((tabs          (funcall tab-bar-tabs-function))
            (tabs-len      (length tabs))
            (current-index (tab-bar--current-tab-index tabs)))
       (unwind-protect
           (progn
             (tab-bar-select-tab ,index)
             ,@body)
         (tab-bar-select-tab (1+ current-index))))))
(put 'with-selected-tab 'lisp-indent-function 2)


(defun ssu-get-current-tab-index (frame)
  (1+ (with-selected-frame frame
        (let ((tabs (funcall tab-bar-tabs-function)))
          (tab-bar--current-tab-index tabs)))))

(defun ssu-set-current-tab-index (index frame)
  (with-selected-frame frame
    (tab-bar-select-tab index)))

(defun ssu-get-buffer-list (tab-index frame)
  (with-selected-tab tab-index frame
    (mapcar #'(lambda (w)
                (let ((b (window-buffer w)))
                  (if (buffer-file-name b)
                      (list :name (buffer-name b)
                            :file (buffer-file-name b)
                            :selected (eq w (selected-window))))))
            (window-list))))

(defun ssu-set-buffer-list (data tab-index frame)
  (with-selected-tab tab-index frame
    (let ((selected-window nil))
      (dolist (d data)
        (find-file-other-window (plist-get d :file))
        (when (plist-get d :selected)
          (setq selected-window (selected-window))))
      (when selected-window
        (select-window selected-window)))))

(defun ssu-get-tab-buffer-list (frame)
  (with-selected-frame frame
    (let* ((tabs    (funcall tab-bar-tabs-function))
           (cidx    (tab-bar--current-tab-index tabs))
           (tab-len (length tabs)))
      (mapcar #'(lambda (i)
                  (ssu-get-buffer-list i frame))
              (number-sequence 1 tab-len)))))

(defun ssu-set-tab-buffer-list (data frame)
  (with-selected-frame frame
    (let ((tabs (funcall tab-bar-tabs-function)))
      (while (> tab-len 0)
        (tab-bar-new-tab-to 6)))))



(defun ssu-get-desktop-buffers (frame)
  (with-selected-frame frame
    (mapcan #'(lambda (screen)
                ;; If nickname exists, use it.
                (setq screen-name (elscreen-get-screen-nickname screen))
                ;; Nickname does not exist, so examine major-mode and buffer-name.
                (when (null screen-name)
                  (elscreen-goto-internal screen)
                  (mapcar #'(lambda (window)
                              (window-buffer window))
                          (window-list))))
            screen-list)))

(defun ssu-set-desktop-buffers (data frame)
  (with-selected-frame frame))


(cl-defmethod sessions-unified--get-frame-data ((app (eql :tab-bar)) frame)
  (with-selected-frame (or nframe (selected-frame))
    (let (fsession-data
          (tabs (funcall tab-bar-tabs-function)))
      (push (cons 'current-tab-index
                  (ssu-get-frame-current-tab-index frame))
            fsession-data)
      (push (cons 'tab-buff-list
                  (ssu-get-frame-tab-buffer-list frame))
            fsession-data)
      (push (cons 'desktop-buffers
                  (ssu-get-desktop-buffers frame))
            fsession-data))))

(cl-defmethod sessions-unified--set-frame-data ((app (eql :tab-bar)) frame  data)
  (let ((fsession-data data))
    (if fsession-data                    ;may causing error
        (progn
          (ssu-set-desktop-buffers         (cdr (assoc 'desktop-buffers fsession-data))
                                           frame)
          (ssu-set-frame-tabs-buffer-list  (cdr (assoc 'tab-buff-list fsession-data))
                                           frame)
          (ssu-set-frame-current-tab-index (cdr (assoc 'current-tab-index fsession-data))
                                           frame)))))

;; (eval-after-load "tab-bar"
;;   (sessions-unified-fsession-register-fns 'tab-bar
;;                                           #'tab-bar-frame-data-get
;;                                           #'tab-bar-frame-data-set))

;;; sessions-unified-tab-bar.el ends here
