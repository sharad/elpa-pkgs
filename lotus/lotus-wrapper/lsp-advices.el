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


(eval-when-compile
  '(require 'dash))


(defun print-otherand-it (lst)
  ;; (message "it: %s, ft it: " it (file-truename it))
  (message "lst: %s" lst)
  lst)

;;;###autoload
(defun lsp-find-session-folder-fn-with-file-truename (session file-name)
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
         ;; (print-otherand-it)
         (--max-by (> (length (and it (file-truename it)))
                      (length (and other (file-truename other))))))))



;; (->> '("aa" "bbb")
;;      (->
;;       (when (message "%s"))))


;;;###autoload
(defun around--lsp-find-session-folder-around-advice-fn-with-file-truename (orgfn &rest args)
  (or (apply orgfn args)
      (apply #'lsp-find-session-folder-fn-with-file-truename args)))

;;; lsp-advices.el ends here
