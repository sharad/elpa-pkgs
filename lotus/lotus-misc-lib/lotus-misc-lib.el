;;; misc-lib.el --- misc lib                         -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sharad

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

(provide 'lotus-misc-lib)


;;;###autoload
(defun lotus-misc-set-old-search-key ()
  (interactive)
  (define-key global-map [remap swiper] 'isearch-forward))
;;;###autoload
(defun lotus-misc-unset-old-search-key ()
  (interactive)
  (define-key global-map [remap swiper] nil))


(defun cleanup-recentf-list ()
  (interactive "ss:")
  (query-replace s1 replacement)
  (recentf-list))

;; /srv/volumes/local/z7mp9s/vg01/lv01/users/s/common/data/main/preserved/Fortinet /home/s/paradise/Projects/Fortinet


(defun xcopy-as-html (beg end)
  (interactive "r")
  ;; Don't let zmacs region highlighting end up in HTML.
  (if (fboundp 'zmacs-deactivate-region)
      (zmacs-deactivate-region)
    (deactivate-mark))
  (let ((htmlbuf (save-restriction
                   (narrow-to-region beg end)
                   (htmlize-buffer-1))))
    (with-current-buffer htmlbuf
      (call-process-region nil nil "xclip" nil nil nil
                           "-i" "-selection" "clipboard" "-t" "text/html"))))

(defun xcopy-as-html (beg end)
  (interactive "r")
  (deactivate-mark)
  (let ((html (htmlize-region-for-paste beg end)))
    (with-temp-buffer
      (insert html)
      (call-process-region nil nil "xclip" nil nil nil
                           "-i" "-selection" "clipboard" "-t" "text/html"))))

(defun xcopy-as-html (beg end)
  (interactive "r")
  (deactivate-mark)
  (let ((html (copy-as-format--html (buffer-substring beg end) t)))
    (with-temp-buffer
      (insert html)
      (call-process-region nil nil "xclip" nil nil nil
                           "-i" "-selection" "clipboard" "-t" "text/html"))))

(defun xcopy-as-html (beg end)
  (interactive "r")
  (deactivate-mark)
  (let* ((htmlize-pre-style t)
         (html (htmlize-region-for-paste beg end)))
    (with-temp-buffer
      (insert html)
      (call-process-region nil nil "xclip" nil nil nil
                           "-i" "-selection" "clipboard" "-t" "text/html"))))


;; Do it in  ghq get git@github.com:sharad/emacs-htmlize.git
;; (defun htmlize-buffer-1 ()
;;  )

;;; misc-lib.el ends here
