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
    (let ((selected-window nil)
          (tdata (copy-tree data)))
      (find-file (plist-get (pop tdata) :file))
      (while tdata
        (let ((d (pop tdata)))
          (find-file-other-window (plist-get d :file))
          (when (plist-get d :selected)
            (setq selected-window (selected-window)))))
      (when selected-window
        (select-window selected-window)))))

(defun ssu-get-tab-buffer-list (frame)
  (let* ((tabs    (funcall tab-bar-tabs-function frame))
         (cidx    (tab-bar--current-tab-index tabs))
         (tab-len (length tabs)))
    (mapcar #'(lambda (i)
                (ssu-get-buffer-list i frame))
            (number-sequence 1 tab-len))))

(defun ssu-set-tab-buffer-list (data frame)
  (let* ((fdata (copy-tree data))
         (tab-len (length fdata))
         (index 1))
    (ssu-set-buffer-list (pop fdata)
                         index
                         frame)
    (while fdata
      (incf index)
      (tab-bar-new-tab)
      (ssu-set-buffer-list (pop fdata)
                           index
                           frame))))

(defun ssu-get-desktop-buffers (frame)

  (let* ((tabs    (funcall tab-bar-tabs-function frame))
         (tab-len (length tabs))
         (buffers (mapcon #'(lambda (tab-index)
                              (with-selected-tab tab-index frame
                                (mapcar #'window-buffer
                                        (window-list))))
                          (number-sequence 1 tab-len))))
    (when buffers
      (remove nil
              (mapcar #'desktop-make-create-buffer-list
                      buffers)))))

(defun ssu-set-desktop-buffers (data frame)
  (let ((desktop-buffers data))
    (dolist (desktop-buffer-args desktop-buffers)
      (let ((bufname (nth 2 desktop-buffer-args))
            (file-path (nth 1 desktop-buffer-args)))
        (session-unfiy-notify "restoring %s" bufname)
        (if (find-buffer-visiting file-path)
            (session-unfiy-notify "buffer %s already here" bufname)
          (if (stringp bufname)
              (if (get-buffer bufname)
                  (session-unfiy-notify "buffer %s already here" bufname)
                (let ()
                  (session-unfiy-notify "Hello 1")
                  (session-unfiy-notify "Desktop lazily opening %s" bufname)
                  (unless (ignore-errors
                            (save-window-excursion
                              (apply 'desktop-create-buffer desktop-buffer-args)))
                    (session-unfiy-notify "Desktop lazily opening Failed."))
                  (session-unfiy-notify "Hello 2")
                  (session-unfiy-notify "restored %s" bufname)))
            (session-unfiy-notify "bufname: %s is not string" bufname)))))))


(cl-defmethod sessions-unified--get-frame-data ((app (eql :tab-bar)) frame)
  (let ((fsession-data nil))
    (push (cons 'current-tab-index
                (ssu-get-current-tab-index frame))
          fsession-data)
    (push (cons 'tab-buff-list
                (ssu-get-tab-buffer-list frame))
          fsession-data)
    (push (cons 'desktop-buffers
                (ssu-get-desktop-buffers frame))
          fsession-data)))


(setq test-atest (sessions-unified--get-frame-data :tab-bar (selected-frame)))

(cl-defmethod sessions-unified--set-frame-data ((app (eql :tab-bar)) frame  data)
  (let ((fsession-data data))
    (if fsession-data                    ;may causing error
        (progn
          (ssu-set-desktop-buffers (cdr (assoc 'desktop-buffers fsession-data))
                                   frame)
          (ssu-set-tab-buffer-list  (cdr (assoc 'tab-buff-list fsession-data))
                                    frame)
          (ssu-set-current-tab-index (cdr (assoc 'current-tab-index fsession-data))
                                     frame)))))

(sessions-unified--set-frame-data :tab-bar
                                  (selected-frame)
                                  test-atest)

;; (eval-after-load "tab-bar"
;;   (sessions-unified-fsession-register-fns 'tab-bar
;;                                           #'tab-bar-frame-data-get
;;                                           #'tab-bar-frame-data-set))

;;; sessions-unified-tab-bar.el ends here
