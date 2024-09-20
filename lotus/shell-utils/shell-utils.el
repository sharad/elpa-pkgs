;;; shell-utils.el --- shell function callables      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Music Player Daemon (MPD) user

;; Author: Music Player Daemon (MPD) user <spratap@merunetworks.com>
;; Keywords: convenience, files, comm, tools

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

;; Include below code in rc file
;;
;; function emacs-client-call-function() {
;;   echo Please define emacs-client-call-function to call function and return result. >&2
;;   return -1
;; }
;; function emacs-source-shell-rcfun() {
;;   local filename="$(emacs-client-call-function shell-rcfun-location)"
;;   if [ "${filename}" ]
;;   then
;;     source ${filename}
;;   fi
;; }
;; emacs-source-shell-rcfun


;;; Code:

(provide 'shell-utils)


(require 'help-fns)

;;;###autoload
(defun shell-current-buffer-filename ()
  (with-current-buffer (window-buffer (selected-window))
    buffer-file-name))

;;;###autoload
(defun shell-current-buffer-filename-truename ()
  (with-current-buffer (window-buffer (selected-window))
    (when buffer-file-name
      (file-truename buffer-file-name))))

;;;###autoload
(defun shell-current-buffer-default-directory ()
  (interactive)
  (with-current-buffer (window-buffer (selected-window))
    default-directory))

;;;###autoload
(defun shell-current-buffer-default-directory-truename ()
  (interactive)
  (with-current-buffer (window-buffer (selected-window))
    (file-name-directory (file-truename (buffer-file-name)))))

;;;###autoload
(defun shell-rcfun-location()
  (let ((file-path (find-lisp-object-file-name 'shell-rcfun-location
                                               'defun)))
    (if file-path
        (expand-file-name "rcfun.sh" (file-name-directory file-path))
      "")))

;;; shell-utils.el ends here
