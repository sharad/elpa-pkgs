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




(defun htmlize-buffer-1 ()
  ;; Internal function; don't call it from outside this file.  Htmlize
  ;; current buffer, writing the resulting HTML to a new buffer, and
  ;; return it.  Unlike htmlize-buffer, this doesn't change current
  ;; buffer or use switch-to-buffer.
  (save-excursion
    ;; Protect against the hook changing the current buffer.
    (save-excursion
      (run-hooks 'htmlize-before-hook))
    ;; Convince font-lock support modes to fontify the entire buffer
    ;; in advance.
    (htmlize-ensure-fontified)
    (clrhash htmlize-extended-character-cache)
    (clrhash htmlize-memoization-table)
    ;; It's important that the new buffer inherits default-directory
    ;; from the current buffer.
    (let ((htmlbuf (generate-new-buffer (if (buffer-file-name)
                                            (htmlize-make-file-name
                                             (file-name-nondirectory
                                              (buffer-file-name)))
                                          "*html*")))
          (completed nil))
      (unwind-protect
          (let* ((buffer-faces (htmlize-faces-in-buffer))
                 (face-map (htmlize-make-face-map
                            (cl-adjoin 'default buffer-faces)))
                 (places (cl-gensym))
                 (title (if (buffer-file-name)
                            (file-name-nondirectory (buffer-file-name))
                          (buffer-name))))
            (when htmlize-generate-hyperlinks
              (htmlize-create-auto-links))
            (when htmlize-replace-form-feeds
              (htmlize-shadow-form-feeds))

            ;; Initialize HTMLBUF and insert the HTML prolog.
            (with-current-buffer htmlbuf
              (buffer-disable-undo)
              (insert (htmlize-method doctype) ?\n
                      (format "<!-- Created by htmlize-%s in %s mode. -->\n"
                              htmlize-version htmlize-output-type)
                      "<html>\n  ")
              (put places 'head-start (point-marker))
              (insert "<head>\n"
                      "    <title>" (htmlize-protect-string title) "</title>\n"
                      (if htmlize-html-charset
                          (format
                           (concat "    <meta http-equiv=\"Content-Type\" "
                                   "content=\"text/html; charset=%s\">\n")
                           htmlize-html-charset)
                        "")
                      htmlize-head-tags)
              (htmlize-method insert-head buffer-faces face-map)
              (insert "  </head>")
              (put places 'head-end (point-marker))
              (insert "\n  ")
              (put places 'body-start (point-marker))
              (insert (htmlize-method body-tag face-map)
                      "\n    ")
              (put places 'content-start (point-marker))
              (insert (htmlize-method pre-tag face-map) "\n"))
            (let ((text-markup
                   ;; Get the inserter method, so we can funcall it inside
                   ;; the loop.  Not calling `htmlize-method' in the loop
                   ;; body yields a measurable speed increase.
                   (htmlize-method-function 'text-markup))
                  ;; Declare variables used in loop body outside the loop
                  ;; because it's faster to establish `let' bindings only
                  ;; once.
                  next-change text face-list trailing-ellipsis
                  fstruct-list last-fstruct-list
                  (close-markup (lambda ())))
              ;; This loop traverses and reads the source buffer, appending
              ;; the resulting HTML to HTMLBUF.  This method is fast
              ;; because: 1) it doesn't require examining the text
              ;; properties char by char (htmlize-next-face-change is used
              ;; to move between runs with the same face), and 2) it doesn't
              ;; require frequent buffer switches, which are slow because
              ;; they rebind all buffer-local vars.
              (goto-char (point-min))
              (while (not (eobp))
                (setq next-change (htmlize-next-face-change (point)))
                ;; Get faces in use between (point) and NEXT-CHANGE, and
                ;; convert them to fstructs.
                (setq face-list (htmlize-faces-at-point)
                      fstruct-list (delq nil (mapcar (lambda (f)
                                                       (gethash f face-map))
                                                     face-list)))
                (cl-multiple-value-setq (text trailing-ellipsis)
                  (htmlize-extract-text (point) next-change trailing-ellipsis))
                ;; Don't bother writing anything if there's no text (this
                ;; happens in invisible regions).
                (when (> (length text) 0)
                  ;; Open the new markup if necessary and insert the text.
                  (when (not (cl-equalp fstruct-list last-fstruct-list))
                    (funcall close-markup)
                    (setq last-fstruct-list fstruct-list)
                    (setq close-markup
                          (funcall text-markup fstruct-list htmlbuf)))

                  (if (string-match-p "\n" text)
                      (dolist (l (split-string text "\\(\n\\)"))
                        (message "l =|%s||" l)
                        (progn ;; unless (string= l "")
                          ;; (if (string= l "")
                          ;;     (princ " " htmlbuf)
                          ;;   (princ l htmlbuf))
                          (princ l htmlbuf)
                          (princ "\n" htmlbuf)
                          (funcall close-markup)
                          (princ "</div style=\"white-space: pre;\">" htmlbuf)
                          (princ "<div>" htmlbuf)
                          (funcall text-markup fstruct-list htmlbuf)))
                    (princ text htmlbuf)))

                (goto-char next-change))

              ;; We've gone through the buffer; close the markup from
              ;; the last run, if any.
              (funcall close-markup))

            ;; Insert the epilog and post-process the buffer.
            (with-current-buffer htmlbuf
              (insert "</div>")
              (put places 'content-end (point-marker))
              (insert "\n  </body>")
              (put places 'body-end (point-marker))
              (insert "\n</html>\n")
              (htmlize-defang-local-variables)
              (goto-char (point-min))
              (when htmlize-html-major-mode
                ;; What sucks about this is that the minor modes, most notably
                ;; font-lock-mode, won't be initialized.  Oh well.
                (funcall htmlize-html-major-mode))
              (set (make-local-variable 'htmlize-buffer-places)
                   (symbol-plist places))
              (run-hooks 'htmlize-after-hook)
              (buffer-enable-undo))
            (setq completed t)
            htmlbuf)

        (when (not completed)
          (kill-buffer htmlbuf))
        (htmlize-delete-tmp-overlays)))))



(if (string-match-p "\n" text)
    (dolist (l (split-string text "\\(\n\\)"))
      (message "l =|%s||" l)
      (progn ;; unless (string= l "")
        (if (string= l "")
            (princ " " htmlbuf)
          (princ l htmlbuf))
        ;; (princ "\n" htmlbuf)
        (funcall close-markup)
        (princ "</div style=\"white-space: pre;\">" htmlbuf)
        (princ "<div>" htmlbuf)
        (funcall text-markup fstruct-list htmlbuf)))
  (princ text htmlbuf))

;;; misc-lib.el ends here
