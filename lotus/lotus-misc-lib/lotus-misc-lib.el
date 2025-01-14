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




(concat "~/.bin/" (file-symlink-p "~/.bin/selsecret"))

(demo-file-truename0 "~/.bin/selsecret")

(defun demo-file-truename0 (filename &optional counter prev-dirs)
  "Return the truename of FILENAME.
If FILENAME is not absolute, first expands it against `default-directory'.
The truename of a file name is found by chasing symbolic links
both at the level of the file and at the level of the directories
containing it, until no links are left at any level.

\(fn FILENAME)"  ;; Don't document the optional arguments.
  ;; COUNTER and PREV-DIRS are used only in recursive calls.
  ;; COUNTER can be a cons cell whose car is the count of how many
  ;; more links to chase before getting an error.
  ;; PREV-DIRS can be a cons cell whose car is an alist
  ;; of truenames we've just recently computed.
  (cond ((or (string= filename "") (string= filename "~"))
         (setq filename (expand-file-name filename))
         (if (string= filename "")
             (setq filename "/")))
        ((and (string= (substring filename 0 1) "~")
              (string-match "~[^/]*/?" filename))
         (let ((first-part
                (substring filename 0 (match-end 0)))
               (rest (substring filename (match-end 0))))
           (setq filename (concat (expand-file-name first-part) rest)))))

  (or counter (setq counter (list 100)))
  (let (done)
    (or prev-dirs (setq prev-dirs (list nil)))
    ;; If this file directly leads to a link, process that iteratively
    ;; so that we don't use lots of stack.
    (while (not done)
      (setcar counter (1- (car counter)))
      (if (< (car counter) 0)
          (error "Apparent cycle of symbolic links for %s" filename))

      (let ((dir (or (file-name-directory filename) default-directory))
            target dirfile)
        (message "m1: %s" dir)

        ;; Get the truename of the directory.
        (setq dirfile (directory-file-name dir))
        ;; If these are equal, we have the (or a) root directory.

        (or (string= dir dirfile)
            (and (file-name-case-insensitive-p dir)
                 (string-equal-ignore-case dir dirfile))
            ;; If this is the same dir we last got the truename for,
            ;; save time--don't recalculate.
            (if (assoc dir (car prev-dirs))
                (setq dir (cdr (assoc dir (car prev-dirs))))
              (let ((old dir)
                    (new (file-name-as-directory (demo-file-truename0 dirfile counter prev-dirs))))
                (setcar prev-dirs (cons (cons old new) (car prev-dirs)))
                (setq dir new))))

        (if (equal ".." (file-name-nondirectory filename))
            (setq filename
                  (directory-file-name (file-name-directory (directory-file-name dir)))
                  done t)
          (if (equal "." (file-name-nondirectory filename))
              (setq filename (directory-file-name dir)
                    done t)
            ;; Put it back on the file name.
            (setq filename (concat dir (file-name-nondirectory filename)))
            ;; Is the file name the name of a link?
            (setq target (file-symlink-p filename))
            (if target
                ;; Yes => chase that link, then start all over
                ;; since the link may point to a directory name that uses links.
                ;; We can't safely use expand-file-name here
                ;; since target might look like foo/../bar where foo
                ;; is itself a link.  Instead, we handle . and .. above.
                (setq filename (files--splice-dirname-file dir target)
                      done nil)
              ;; No, we are done!
              (setq done t))))))
    filename))


(defun demo-file-truename1 (filename trg &optional counter prev-dirs)
  "Return the truename of FILENAME.
If FILENAME is not absolute, first expands it against `default-directory'.
The truename of a file name is found by chasing symbolic links
both at the level of the file and at the level of the directories
containing it, until no links are left at any level.

\(fn FILENAME)"  ;; Don't document the optional arguments.
  ;; COUNTER and PREV-DIRS are used only in recursive calls.
  ;; COUNTER can be a cons cell whose car is the count of how many
  ;; more links to chase before getting an error.
  ;; PREV-DIRS can be a cons cell whose car is an alist
  ;; of truenames we've just recently computed.
  (cond ((or (string= filename "") (string= filename "~"))
         (setq filename (expand-file-name filename))
         (if (string= filename "")
             (setq filename "/")))
        ((and (string= (substring filename 0 1) "~")
              (string-match "~[^/]*/?" filename))
         (let ((first-part
                (substring filename 0 (match-end 0)))
               (rest (substring filename (match-end 0))))
           (setq filename (concat (expand-file-name first-part) rest)))))

  (or counter (setq counter (list 100)))
  (let (done
        trgdir)
    (or prev-dirs (setq prev-dirs (list nil)))
    ;; If this file directly leads to a link, process that iteratively
    ;; so that we don't use lots of stack.
    (while (not done)
      (setcar counter (1- (car counter)))
      (if (< (car counter) 0)
          (error "Apparent cycle of symbolic links for %s" filename))

      (let ((dir (or (file-name-directory filename) default-directory))
            target dirfile)
        (message "m1: %s" dir)

        ;; Get the truename of the directory.
        (setq dirfile (directory-file-name dir))
        ;; If these are equal, we have the (or a) root directory.

        (or (string= dir dirfile)
            (and (file-name-case-insensitive-p dir)
                 (string-equal-ignore-case dir dirfile))
            ;; If this is the same dir we last got the truename for,
            ;; save time--don't recalculate.
            (if (assoc dir (car prev-dirs))
                (setq dir (cdr (assoc dir (car prev-dirs))))
              (message "before trgdir = %s" trgdir)
              (let* ((old dir)
                     (file-trgdir (demo-file-truename1 dirfile trg counter prev-dirs))
                     (new (file-name-as-directory (car file-trgdir))))
                (when (file-exists-p (expand-file-name trg (cdr file-trgdir)))
                  (message "after trgdir = %s" (cdr file-trgdir))
                  (setq trgdir (cdr file-trgdir)))
                (setcar prev-dirs (cons (cons old new) (car prev-dirs)))
                (setq dir new))))

        (if (equal ".." (file-name-nondirectory filename))
            (setq filename
                  (directory-file-name (file-name-directory (directory-file-name dir)))
                  done t)
          (if (equal "." (file-name-nondirectory filename))
              (setq filename (directory-file-name dir)
                    done t)
            ;; Put it back on the file name.
            (setq filename (concat dir (file-name-nondirectory filename)))
            ;; Is the file name the name of a link?
            (setq target (file-symlink-p filename))
            (if target
                ;; Yes => chase that link, then start all over
                ;; since the link may point to a directory name that uses links.
                ;; We can't safely use expand-file-name here
                ;; since target might look like foo/../bar where foo
                ;; is itself a link.  Instead, we handle . and .. above.
                (setq filename (files--splice-dirname-file dir target)
                      done nil)
              ;; No, we are done!
              (setq done t))))))
    (cons filename trgdir)))



(defun demo-file-truename2 (filename trg &optional counter prev-dirs)
  "Return the truename of FILENAME.
If FILENAME is not absolute, first expands it against `default-directory'.
The truename of a file name is found by chasing symbolic links
both at the level of the file and at the level of the directories
containing it, until no links are left at any level.

\(fn FILENAME)"  ;; Don't document the optional arguments.
  ;; COUNTER and PREV-DIRS are used only in recursive calls.
  ;; COUNTER can be a cons cell whose car is the count of how many
  ;; more links to chase before getting an error.
  ;; PREV-DIRS can be a cons cell whose car is an alist
  ;; of truenames we've just recently computed.
  (cond ((or (string= filename "") (string= filename "~"))
         (setq filename (expand-file-name filename))
         (if (string= filename "")
             (setq filename "/")))
        ((and (string= (substring filename 0 1) "~")
              (string-match "~[^/]*/?" filename))
         (let ((first-part
                (substring filename 0 (match-end 0)))
               (rest (substring filename (match-end 0))))
           (setq filename (concat (expand-file-name first-part) rest)))))

  (or counter (setq counter (list 100)))
  (let (done
        trgdir)
    (or prev-dirs (setq prev-dirs (list nil)))
    ;; If this file directly leads to a link, process that iteratively
    ;; so that we don't use lots of stack.
    (while (not done)
      (setcar counter (1- (car counter)))
      (if (< (car counter) 0)
          (error "Apparent cycle of symbolic links for %s" filename))

      (let ((dir (or (file-name-directory filename) default-directory))
            target dirfile)
        (message "m%d: filename = %s dir %s"
                 (or (car counter) 0) filename dir)

        ;; Get the truename of the directory.
        (setq dirfile (directory-file-name dir))
        ;; If these are equal, we have the (or a) root directory.

        (or (string= dir dirfile)
            (and (file-name-case-insensitive-p dir)
                 (string-equal-ignore-case dir dirfile))
            ;; If this is the same dir we last got the truename for,
            ;; save time--don't recalculate.
            (if (assoc dir (car prev-dirs))
                (setq dir (cdr (assoc dir (car prev-dirs))))
              (message "m%d: before trgdir = %s" (or (car counter) 0) trgdir)
              (let* ((old dir)
                     (file-trgdir (demo-file-truename2 dirfile trg counter prev-dirs))
                     (new (file-name-as-directory (car file-trgdir))))
                (when (file-exists-p (expand-file-name trg (cdr file-trgdir)))
                  (message "m%d: after trgdir = %s" (or (car counter) 0) (cdr file-trgdir))
                  (setq trgdir (cdr file-trgdir)))
                (setcar prev-dirs (cons (cons old new) (car prev-dirs)))
                (setq dir new))))

        (if (equal ".." (file-name-nondirectory filename))
            (setq filename
                  (directory-file-name (file-name-directory (directory-file-name dir)))
                  done t)
          (if (equal "." (file-name-nondirectory filename))
              (setq filename (directory-file-name dir)
                    done t)
            ;; Put it back on the file name.
            (setq filename (concat dir (file-name-nondirectory filename)))
            ;; Is the file name the name of a link?
            (setq target (file-symlink-p filename))
            (if target
                ;; Yes => chase that link, then start all over
                ;; since the link may point to a directory name that uses links.
                ;; We can't safely use expand-file-name here
                ;; since target might look like foo/../bar where foo
                ;; is itself a link.  Instead, we handle . and .. above.
                (setq filename (files--splice-dirname-file dir target)
                      done nil)
              ;; No, we are done!
              (setq done t))))))
    (cons filename trgdir)))


(defun demo-file-truename3 (filename trg &optional counter prev-dirs trgdirs)
  "Return the truename of FILENAME.
If FILENAME is not absolute, first expands it against `default-directory'.
The truename of a file name is found by chasing symbolic links
both at the level of the file and at the level of the directories
containing it, until no links are left at any level.

\(fn FILENAME)"  ;; Don't document the optional arguments.
  ;; COUNTER and PREV-DIRS are used only in recursive calls.
  ;; COUNTER can be a cons cell whose car is the count of how many
  ;; more links to chase before getting an error.
  ;; PREV-DIRS can be a cons cell whose car is an alist
  ;; of truenames we've just recently computed.
  (cond ((or (string= filename "") (string= filename "~"))
         (setq filename (expand-file-name filename))
         (if (string= filename "")
             (setq filename "/")))
        ((and (string= (substring filename 0 1) "~")
              (string-match "~[^/]*/?" filename))
         (let ((first-part
                (substring filename 0 (match-end 0)))
               (rest (substring filename (match-end 0))))
           (setq filename (concat (expand-file-name first-part) rest)))))

  (or counter (setq counter (list 100)))
  (let (done)
    (or prev-dirs (setq prev-dirs (list nil)))
    ;; If this file directly leads to a link, process that iteratively
    ;; so that we don't use lots of stack.
    (while (not done)
      (setcar counter (1- (car counter)))
      (if (< (car counter) 0)
          (error "Apparent cycle of symbolic links for %s" filename))

      (let ((dir (or (file-name-directory filename) default-directory))
            target dirfile)
        (message "m%d: filename = %s dir %s"
                 (or (car counter) 0) filename dir)

        (when (file-exists-p (expand-file-name trg dir))
          ;; (message "m%d: found trgdir = %s" (or (car counter) 0) dir)
          (setq trgdirs (append trgdirs (list dir))))

        ;; Get the truename of the directory.
        (setq dirfile (directory-file-name dir))
        ;; If these are equal, we have the (or a) root directory.

        (or (string= dir dirfile)
            (and (file-name-case-insensitive-p dir)
                 (string-equal-ignore-case dir dirfile))
            ;; If this is the same dir we last got the truename for,
            ;; save time--don't recalculate.
            (if (assoc dir (car prev-dirs))
                (setq dir (cdr (assoc dir (car prev-dirs))))
              ;; (message "m%d: before trgdir = %s" (or (car counter) 0) trgdirs)
              (let* ((old dir)
                     (file-trgdir (demo-file-truename3 dirfile trg counter prev-dirs trgdirs))
                     (new (file-name-as-directory (car file-trgdir))))
                (setq trgdirs (cdr file-trgdir))
                (setcar prev-dirs (cons (cons old new) (car prev-dirs)))
                (setq dir new))))

        (if (equal ".." (file-name-nondirectory filename))
            (setq filename
                  (directory-file-name (file-name-directory (directory-file-name dir)))
                  done t)
          (if (equal "." (file-name-nondirectory filename))
              (setq filename (directory-file-name dir)
                    done t)
            ;; Put it back on the file name.
            (setq filename (concat dir (file-name-nondirectory filename)))
            ;; Is the file name the name of a link?
            (setq target (file-symlink-p filename))
            (if target
                ;; Yes => chase that link, then start all over
                ;; since the link may point to a directory name that uses links.
                ;; We can't safely use expand-file-name here
                ;; since target might look like foo/../bar where foo
                ;; is itself a link.  Instead, we handle . and .. above.
                (setq filename (files--splice-dirname-file dir target)
                      done nil)
              ;; No, we are done!
              (setq done t))))))
    (cons filename trgdirs)))

;; (demo-file-truename0 "~/.bin/selsecret")
;; (demo-file-truename1 "~/.bin/selsecret" ".git")
;; (demo-file-truename2 "~/.bin/selsecret" ".git")
;; (demo-file-truename3 "~/.bin/selsecret" ".git")


(defun locate-dominating-file-dirs (filename name &optional counter prev-dirs trgdirs)
  "Return the truename of FILENAME.
If FILENAME is not absolute, first expands it against `default-directory'.
The truename of a file name is found by chasing symbolic links
both at the level of the file and at the level of the directories
containing it, until no links are left at any level.

\(fn FILENAME)"  ;; Don't document the optional arguments.
  ;; COUNTER and PREV-DIRS are used only in recursive calls.
  ;; COUNTER can be a cons cell whose car is the count of how many
  ;; more links to chase before getting an error.
  ;; PREV-DIRS can be a cons cell whose car is an alist
  ;; of truenames we've just recently computed.
  (cond ((or (string= filename "") (string= filename "~"))
         (setq filename (expand-file-name filename))
         (if (string= filename "")
             (setq filename "/")))
        ((and (string= (substring filename 0 1) "~")
              (string-match "~[^/]*/?" filename))
         (let ((first-part
                (substring filename 0 (match-end 0)))
               (rest (substring filename (match-end 0))))
           (setq filename (concat (expand-file-name first-part) rest)))))

  (or counter (setq counter (list 100)))
  (let (done)
    (or prev-dirs (setq prev-dirs (list nil)))
    ;; If this file directly leads to a link, process that iteratively
    ;; so that we don't use lots of stack.
    (while (not done)
      (setcar counter (1- (car counter)))
      (if (< (car counter) 0)
          (error "Apparent cycle of symbolic links for %s" filename))

      (let ((dir (or (file-name-directory filename) default-directory))
            target
            dirfile)
        (when (if (functionp name)
                  (funcall name dir)
                (file-exists-p (expand-file-name name dir)))
          (setq trgdirs (append trgdirs (list dir))))

        ;; Get the truename of the directory.
        (setq dirfile (directory-file-name dir))
        ;; If these are equal, we have the (or a) root directory.

        (or (string= dir dirfile)
            (and (file-name-case-insensitive-p dir)
                 (string-equal-ignore-case dir dirfile))
            ;; If this is the same dir we last got the truename for,
            ;; save time--don't recalculate.
            (if (assoc dir (car prev-dirs))
                (setq dir (cdr (assoc dir (car prev-dirs))))
              ;; (message "m%d: before trgdir = %s" (or (car counter) 0) trgdirs)
              (let* ((old dir)
                     (file-trgdir (locate-dominating-file-dirs dirfile name counter prev-dirs trgdirs))
                     (new (file-name-as-directory (car file-trgdir))))
                (setq trgdirs (cdr file-trgdir))
                (setcar prev-dirs (cons (cons old new) (car prev-dirs)))
                (setq dir new))))

        (if (equal ".." (file-name-nondirectory filename))
            (setq filename
                  (directory-file-name (file-name-directory (directory-file-name dir)))
                  done t)
          (if (equal "." (file-name-nondirectory filename))
              (setq filename (directory-file-name dir)
                    done t)
            ;; Put it back on the file name.
            (setq filename (concat dir (file-name-nondirectory filename)))
            ;; Is the file name the name of a link?
            (setq target (file-symlink-p filename))
            (if target
                ;; Yes => chase that link, then start all over
                ;; since the link may point to a directory name that uses links.
                ;; We can't safely use expand-file-name here
                ;; since target might look like foo/../bar where foo
                ;; is itself a link.  Instead, we handle . and .. above.
                (setq filename (files--splice-dirname-file dir target)
                      done nil)
              ;; No, we are done!
              (setq done t))))))
    (cons filename trgdirs)))


(defun locate-dominating-file-dir (filename name)
  (cadr (locate-dominating-file-dirs filename name)))

(when nil
  (locate-dominating-file-dir "~/.zshrc" ".git")

  (locate-dominating-file (file-name-directory file)
                          #'dir-locals--all-files)

  (dir-locals-find-file "~/.bin/selsecret")
  (dir-locals-find-file "~/.zshrc")

  (locate-dominating-file-dir "~/.zshrc" ".dir-locals.el")
  (locate-dominating-file-dir "~/.zshrc" #'dir-locals--all-files)
  (locate-dominating-file-dir "~/.zshrc" #'dir-locals--all-files))

(defun dir-locals-find-file-around (orgfn &rest args)
  (let ((r (apply orgfn args)))
    (message "Dir-org returned: %s args = %S" r args)
    (if r
        r
      (let ((nr (apply #'locate-dominating-file-dir (append args (list #'dir-locals--all-files)))))
        (message "New Dir-around returned: %s" nr)
        nr))))

(advice-add 'dir-locals-find-file :around #'dir-locals-find-file-around)
(advice-remove 'dir-locals-find-file #'dir-locals-find-file-around)

;; (defun his-tracing-function (orig-fun &rest args)
;;   (message "display-buffer called with args %S" args)
;;   (let ((res (apply orig-fun args)))
;;     (message "display-buffer returned %S" res)
;;     res))

;; (advice-add 'display-buffer :around #'his-tracing-function)
;; (advice-remove 'display-buffer #'his-tracing-function)

;;; misc-lib.el ends here
