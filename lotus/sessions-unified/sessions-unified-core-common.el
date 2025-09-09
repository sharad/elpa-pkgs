;;; sessions-unified-core-common.el --- common code  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Music Player Daemon (MPD) user

;; Author: Music Player Daemon (MPD) user <spratap@merunetworks.com>
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

(provide 'sessions-unified-core-common)


(defvar session-unified-dir "~/.emacs.d/.cache/session-unified/")

(defvar session-unified-debug nil)

(defvar *sessions-unified-utils-notify* nil)


(cl-defgeneric sessions-unified--session-store (app))
(cl-defgeneric sessions-unified--session-restore (app))
(cl-defgeneric sessions-unified--session-enable (app))
(cl-defgeneric sessions-unified--session-disable (app))
(cl-defgeneric sessions-unified--session-check (app))



(cl-defgeneric sessions-unified--get-frame-data (app frame)
  "sessions-unified--get-frame-data")
(cl-defgeneric sessions-unified--set-frame-data (app frame data)
  "sessions-unified--set-frame-data")


(defun make-session-unified-dir (&optional path)
  (let ((dir (if path
                 (expand-file-name path session-unified-dir)
               session-unified-dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(make-session-unified-dir)
(setq session-save-file (expand-file-name "session/session.el" session-unified-dir))


(defun sessions-unified-utils-notify-default (title fmt &rest args)
  (message  "%s: %s" title (apply #'format fmt args)))

;; https://emacs.stackexchange.com/questions/2310/can-functions-access-their-name
(defun get-current-func-name ()
  "Get the symbol of the function this function is called from."
  ;; 5 is the magic number that makes us look
  ;; above this function
  ;; (message "start get-current-func-name-debug")
  (let* ((limit 50)
         (index 4)
         (frame (backtrace-frame index)))
    ;; from what I can tell, top level function call frames
    ;; start with t and the second value is the symbol of the function
    ;; (message "b4 while")
    (while (and (not (equal t (car frame)))
                (< index limit))
      ;; (message "while loop index %d" index)
      (setq frame (backtrace-frame (cl-incf index))))
    ;; (message "completed while frame")
    (if (equal t (car frame))
        (let ((fun (second frame)))
          (if (symbolp fun)
              (symbol-name fun)
            (format "%s" fun)))
      "unknown")))

(defun session-unify-notify (fmt &rest args)
  (let ((funname (get-current-func-name)))
    ;; (message "test")
    (let ((notify (or *sessions-unified-utils-notify*
                      #'sessions-unified-utils-notify-default)))
      (unless (eq notify
                  #'sessions-unified-utils-notify-default)
        (apply #'sessions-unified-utils-notify-default funname fmt args))
      (apply notify funname fmt args))))

;; (session-unify-notify "Enabled session saving")
;; (apply *sessions-unified-utils-notify* "test" "fmt" '())


;;;###autoload
(defun protable-display-graphic-p ()
  (if (< emacs-major-version 24)
      (eq (frame-parameter (selected-frame) 'window-system)
          'x)
    (display-graphic-p)))



;;; sessions-unified-core-common.el ends here
