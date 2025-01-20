;;; lsp-advices.el --- Lsp advices                   -*- lexical-binding: t; -*-

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

(provide 'lsp-advices)


;;;###autoload
(defun lsp-find-session-folder-around-advice-fn-with-file-truename (orgfn &rest args)
  (or (apply orgfn args)
      (let ((session   (car args))
            (file-name (cadr args)))
        (let ((file-name-canonical (lsp-f-canonical (file-truename file-name))))
          (->> session
               (lsp-session-folders)
               (--filter (and (lsp--files-same-host (file-truename it)
                                                    file-name-canonical)
                              (or (lsp-f-same? (file-truename it)
                                               file-name-canonical)
                                  (and (f-dir? (file-truename it))
                                       (lsp-f-ancestor-of? (file-truename it)
                                                           file-name-canonical)))))
               (--max-by (> (length (file-truename it))
                            (length (file-truename other)))))))))

;;; lsp-advices.el ends here
