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
;; function source-emacs-shell-rcfun() {
;;     local filename=${(Q)~$(emacsclient -f ~/.emacs.d/server/$EMACS_SERVER_NAME \
;;                                        -w 2                                    \
;;                                        -e '(shell-rcfun-location)' |           \
;;                                        sed -e '1!b' -e '/emacsclient: connected to remote socket at/d')}
;;     if [ "${filename}" ]
;;     then
;;         source ${filename}
;;     fi
;; }
;; source-emacs-shell-rcfun


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
(defun shell-current-buffer-directory ()
  (interactive)
  (with-current-buffer (window-buffer (selected-window))
    default-directory))

;;;###autoload
(defun shell-current-buffer-directory-truename ()
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
