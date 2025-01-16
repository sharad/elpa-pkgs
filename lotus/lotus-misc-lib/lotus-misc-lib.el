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

  (or counter (setq counter (list 300)))
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

  (dir-locals-find-file (buffer-file-name))

  (locate-dominating-file-dir (buffer-file-name) ".dir-locals.el")

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

(defun dir-locals-find-file-around (orgfn &rest args)
  (or (apply orgfn args)
      (apply #'locate-dominating-file-dir (append args (list #'dir-locals--all-files)))))

(advice-remove 'dir-locals-find-file
               #'dir-locals-find-file-around)
(advice-add 'dir-locals-find-file
            :around #'dir-locals-find-file-around)
(advice--p #'dir-locals-find-file)


(defun bury-previous-minibuffers ()
  (let ((blist (cdr (buffer-list))))
    (while (string-match-p " \\*Minibuf"
                           (buffer-name (car blist)))
      (bury-buffer (car blist))
      (message "Buried %s buffer"
               (car blist))
      (setq blist (cdr blist)))))

(remove-hook 'kill-buffer-hook
             #'bury-previous-minibuffers)
(add-hook 'kill-buffer-hook
          #'bury-previous-minibuffers)



;; interactive root checking done in lsp--try-project-root-workspaces

;; (defun lsp-f-same? (path-a path-b)
;;   "Return t if PATH-A and PATH-B are references to the same file.
;; Symlinks are not followed."
;;   (when (and (f-exists? path-a)
;;              (f-exists? path-b))
;;     (equal
;;      (lsp-f-canonical (directory-file-name (f-expand path-a)))
;;      (lsp-f-canonical (directory-file-name (f-expand path-b))))))

;; (defun lsp-f-ancestor-of? (path-a path-b)
;;   "Return t if PATH-A is an ancestor of PATH-B.
;; Symlinks are not followed."
;;   (unless (lsp-f-same? path-a path-b)
;;     (s-prefix? (concat (lsp-f-canonical path-a) (f-path-separator))
;;                (lsp-f-canonical path-b))))

;; (defun lsp-find-session-folder (session file-name)
;;   "Look in the current SESSION for folder containing FILE-NAME."
;;   (let ((file-name-canonical (lsp-f-canonical file-name)))
;;     (->> session
;;          (lsp-session-folders)
;;          (--filter (and (lsp--files-same-host it file-name-canonical)
;;                         (or (lsp-f-same? it file-name-canonical)
;;                             (and (f-dir? it)
;;                                  (lsp-f-ancestor-of? it file-name-canonical)))))
;;          (--max-by (> (length it)
;;                       (length other))))))


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
(advice-remove 'lsp-find-session-folder
               #'lsp-find-session-folder-around-advice-fn-with-file-truename)
(advice-add 'lsp-find-session-folder :around
            #'lsp-find-session-folder-around-advice-fn-with-file-truename)


;; TEST (lsp-find-session-folder (lsp-session) (buffer-file-name) )

;; (when nil

;;   (defun lsp--calculate-root (session file-name)
;;     "Calculate project root for FILE-NAME in SESSION."
;;     (and
;;      (->> session
;;           (lsp-session-folders-blocklist)
;;           (--first (and (lsp--files-same-host it file-name)
;;                         (lsp-f-ancestor-of? it file-name)
;;                         (prog1 t
;;                           (lsp--info "File %s is in blocklisted directory %s" file-name it))))
;;           not)
;;      (or
;;       (when lsp-auto-guess-root
;;         (lsp--suggest-project-root))
;;       (unless lsp-guess-root-without-session
;;         (lsp-find-session-folder session file-name))
;;       (unless lsp-auto-guess-root
;;         (when-let* ((root-folder (lsp--find-root-interactively session)))
;;           (if (or (not (f-equal? root-folder (expand-file-name "~/")))
;;                   (yes-or-no-p
;;                    (concat
;;                     (propertize "[WARNING] " 'face 'warning)
;;                     "You are trying to import your home folder as project root. This may cause performance issue because some language servers (python, lua, etc) will try to scan all files under project root. To avoid that you may:

;; 1. Use `I' option from the interactive project import to select subfolder(e. g. `~/foo/bar' instead of `~/').
;; 2. If your file is under `~/' then create a subfolder and move that file in this folder.

;; Type `No' to go back to project selection.
;; Type `Yes' to confirm `HOME' as project root.
;; Type `C-g' to cancel project import process and stop `lsp'")))
;;               root-folder
;;             (lsp--calculate-root session file-name)))))))


;;   (defun lsp--try-project-root-workspaces (ask-for-client ignore-multi-folder)
;;     "Try create opening file as a project file.
;; When IGNORE-MULTI-FOLDER is t the lsp mode will start new
;; language server even if there is language server which can handle
;; current language. When IGNORE-MULTI-FOLDER is nil current file
;; will be opened in multi folder language server if there is
;; such."
;;     (-let ((session (lsp-session)))
;;       (-if-let (clients (if ask-for-client
;;                             (list (lsp--completing-read "Select server to start: "
;;                                                         (ht-values lsp-clients)
;;                                                         (-compose 'symbol-name 'lsp--client-server-id) nil t))
;;                           (lsp--find-clients)))
;;           (-if-let (project-root (-some-> session
;;                                    (lsp--calculate-root (buffer-file-name))
;;                                    (lsp-f-canonical)))
;;               (progn
;;                 ;; update project roots if needed and persist the lsp session
;;                 (unless (-contains? (lsp-session-folders session) project-root)
;;                   (cl-pushnew project-root (lsp-session-folders session))
;;                   (lsp--persist-session session))
;;                 (lsp--ensure-lsp-servers session clients project-root ignore-multi-folder))
;;             (lsp--warn "%s not in project or it is blocklisted." (buffer-name))
;;             nil)
;;         (lsp--warn "No LSP server for %s(check *lsp-log*)." major-mode)
;;         nil)))

;;   (setq lsp--buffer-workspaces
;;         (or (and
;;              ;; Don't open as library file if file is part of a project.
;;              (not (lsp-find-session-folder (lsp-session) (buffer-file-name)))
;;              (lsp--try-open-in-library-workspace))
;;             (lsp--try-project-root-workspaces (equal arg '(4))
;;                                               (and arg (not (equal arg 1))))))


;;   (defun lsp (&optional arg)
;;     "Entry point for the server startup.
;; When ARG is t the lsp mode will start new language server even if
;; there is language server which can handle current language. When
;; ARG is nil current file will be opened in multi folder language
;; server if there is such. When `lsp' is called with prefix
;; argument ask the user to select which language server to start."
;;     (interactive "P")

;;     (lsp--require-packages)

;;     (when (buffer-file-name)
;;       (let (clients
;;             (matching-clients (lsp--filter-clients
;;                                (-andfn #'lsp--supports-buffer?
;;                                        #'lsp--server-binary-present?))))
;;         (cond
;;          (matching-clients
;;           (when (setq lsp--buffer-workspaces
;;                       (or (and
;;                            ;; Don't open as library file if file is part of a project.
;;                            (not (lsp-find-session-folder (lsp-session) (buffer-file-name)))
;;                            (lsp--try-open-in-library-workspace))
;;                           (lsp--try-project-root-workspaces (equal arg '(4))
;;                                                             (and arg (not (equal arg 1))))))
;;             (lsp-mode 1)
;;             (when lsp-auto-configure (lsp--auto-configure))
;;             (setq lsp-buffer-uri (lsp--buffer-uri))
;;             (lsp--info "Connected to %s."
;;                        (apply 'concat (--map (format "[%s %s]"
;;                                                      (lsp--workspace-print it)
;;                                                      (lsp--workspace-root it))
;;                                              lsp--buffer-workspaces)))))
;;          ;; look for servers which are currently being downloaded.
;;          ((setq clients (lsp--filter-clients (-andfn #'lsp--supports-buffer?
;;                                                      #'lsp--client-download-in-progress?)))
;;           (lsp--info "There are language server(%s) installation in progress.
;; The server(s) will be started in the buffer when it has finished."
;;                      (-map #'lsp--client-server-id clients))
;;           (seq-do (lambda (client)
;;                     (cl-pushnew (current-buffer) (lsp--client-buffers client)))
;;                   clients))
;;          ;; look for servers to install
;;          ((setq clients (lsp--filter-clients
;;                          (-andfn #'lsp--supports-buffer?
;;                                  (-const lsp-enable-suggest-server-download)
;;                                  #'lsp--client-download-server-fn
;;                                  (-not #'lsp--client-download-in-progress?))))
;;           (let ((client (lsp--completing-read
;;                          (concat "Unable to find installed server supporting this file. "
;;                                  "The following servers could be installed automatically: ")
;;                          clients
;;                          (-compose #'symbol-name #'lsp--client-server-id)
;;                          nil
;;                          t)))
;;             (cl-pushnew (current-buffer) (lsp--client-buffers client))
;;             (lsp--install-server-internal client)))
;;          ;; ignore other warnings
;;          ((not lsp-warn-no-matched-clients)
;;           nil)
;;          ;; automatic installation disabled
;;          ((setq clients (unless matching-clients
;;                           (lsp--filter-clients (-andfn #'lsp--supports-buffer?
;;                                                        #'lsp--client-download-server-fn
;;                                                        (-not (-const lsp-enable-suggest-server-download))
;;                                                        (-not #'lsp--server-binary-present?)))))
;;           (lsp--warn "The following servers support current file but automatic download is disabled: %s
;; \(If you have already installed the server check *lsp-log*)."
;;                      (mapconcat (lambda (client)
;;                                   (symbol-name (lsp--client-server-id client)))
;;                                 clients
;;                                 " ")))
;;          ;; no clients present
;;          ((setq clients (unless matching-clients
;;                           (lsp--filter-clients (-andfn #'lsp--supports-buffer?
;;                                                        (-not #'lsp--server-binary-present?)))))
;;           (lsp--warn "The following servers support current file but do not have automatic installation: %s
;; You may find the installation instructions at https://emacs-lsp.github.io/lsp-mode/page/languages.
;; \(If you have already installed the server check *lsp-log*)."
;;                      (mapconcat (lambda (client)
;;                                   (symbol-name (lsp--client-server-id client)))
;;                                 clients
;;                                 " ")))
;;          ;; no matches
;;          ((-> #'lsp--supports-buffer? lsp--filter-clients not)
;;           (lsp--error "There are no language servers supporting current mode `%s' registered with `lsp-mode'.
;; This issue might be caused by:
;; 1. The language you are trying to use does not have built-in support in `lsp-mode'. You must install the required support manually. Examples of this are `lsp-java' or `lsp-metals'.
;; 2. The language server that you expect to run is not configured to run for major mode `%s'. You may check that by checking the `:major-modes' that are passed to `lsp-register-client'.
;; 3. The language server that you expect to run has an `:activation-fn` passed to `lsp-register-client` that prevents it supporting this buffer.
;; 4. `lsp-mode' doesn't have any integration for the language behind `%s'. Refer to https://emacs-lsp.github.io/lsp-mode/page/languages and https://langserver.org/ .
;; 5. You are over `tramp'. In this case follow https://emacs-lsp.github.io/lsp-mode/page/remote/.
;; 6. You have disabled the `lsp-mode' clients for that file. (Check `lsp-enabled-clients' and `lsp-disabled-clients').
;; You can customize `lsp-warn-no-matched-clients' to disable this message."
;;                       major-mode major-mode major-mode)))))))


;; (dir-locals-find-file "~/.zshrc")
;; (dir-locals-find-file (or (buffer-file-name) default-directory))
;; (dir-locals-read-from-dir dir-or-cache)

;; (dir-locals-read-from-dir (dir-locals-find-file "~/.zshrc"))


(defun dir-locals-collect-variables-fn-with-file-truename (class-variables root variables
                                                                           &optional predicate)
  "Collect entries from CLASS-VARIABLES into VARIABLES.
ROOT is the root directory of the project.
Return the new variables list.
If PREDICATE is given, it is used to test a symbol key in the alist
to see whether it should be considered."
  (let* ((file-name (or (buffer-file-name)
                        ;; Handle non-file buffers, too.
                        (expand-file-name default-directory)))
         (sub-file-name (if (and file-name
                                 (file-name-absolute-p file-name))
                            ;; FIXME: Why not use file-relative-name?
                            (substring (file-truename file-name)
                                       (length (file-truename root))))))
    (condition-case err
        (dolist (entry class-variables variables)
          (let ((key (car entry)))
            (cond
             ((stringp key)
              ;; Don't include this in the previous condition, because we
              ;; want to filter all strings before the next condition.
              (when (and sub-file-name
                         (>= (length sub-file-name) (length key))
                         (string-prefix-p key sub-file-name))
                (setq variables (dir-locals-collect-variables
                                 (cdr entry) root variables predicate))))
             ((if predicate
                  (funcall predicate key)
                (or (not key)
                    (derived-mode-p key)))
              (let* ((alist (cdr entry))
                     (subdirs (assq 'subdirs alist)))
                (if (or (not subdirs)
                        (progn
                          (setq alist (remq subdirs alist))
                          (cdr-safe subdirs))
                        ;; TODO someone might want to extend this to allow
                        ;; integer values for subdir, where N means
                        ;; variables apply to this directory and N levels
                        ;; below it (0 == nil).
                        (equal root (expand-file-name default-directory)))
                    (setq variables (dir-locals-collect-mode-variables
                                     alist variables))))))))
      (error
       ;; The file's content might be invalid (e.g. have a merge conflict), but
       ;; that shouldn't prevent the user from opening the file.
       (message "%s error: %s" dir-locals-file (error-message-string err))
       nil))))
(defun dir-locals-collect-variables-around-advice-fn-with-file-truename (orgfn &rest args)
  (condition-case err
      (apply orgfn args)
    (args-out-of-range
     (let ((file-name (cadr err)))
       (if (string= file-name (or (buffer-file-name)
                                  (expand-file-name default-directory)))
           (apply #'dir-locals-collect-variables-fn-with-file-truename
                  args)
         (error err))))))
(advice-remove 'dir-locals-collect-variables
               #'dir-locals-collect-variables-around-advice-fn-with-file-truename)
(advice-add 'dir-locals-collect-variables :around
            #'dir-locals-collect-variables-around-advice-fn-with-file-truename)


(defun magit-wip-push (ref &optional remote args)
  (interactive
   (list (magit-ref-fullname "HEAD")
         (magit-get-upstream-remote (magit-get-current-branch))
         (when current-prefix-arg '("-f"))))
  (let* ((local-branch    (substring ref (length "refs/heads/")))
         (upstream-remote (or remote
                              (magit-get-upstream-remote local-branch)))
         (wip-ref (concat "wip/wtree/" ref))
         (local-wip-ref (string-join (list "refs" wip-ref) "/")))
    (if (magit-ref-p local-wip-ref)
        (if (magit-git-push local-wip-ref
                            (string-join (list upstream-remote wip-ref) "/")
                            args)
            (message "push passed")
          (message "push failed"))
      (message "magit-wip-push: ref %s not exists"
               wip-ref))))
(defun magit-wip-commit-worktree-fn-to-push-wip (ref files msg)
  (message "magit-wip-push: ref %s, files %s, msg %s"
           ref files msg)
  (magit-wip-push ref))
(defun magit-wip-commit-worktree-around-advice-fn (orgfn &rest args)
  (progn
    (if (apply orgfn args)
        (message "magit-wip-commit-worktree-around success")
      (message "magit-wip-commit-worktree-around fail"))
    (message "args: %S" args)
    (apply #'magit-wip-commit-worktree-fn-to-push-wip
           args)))

;; Define the global minor mode for magit wip push
(define-minor-mode magit-wip-push-mode
  "A global minor mode magit wip push."
  :global t                             ; Make it a global minor mode
  :init-value nil                       ; Default to disabled
  :lighter " WP"                        ; Display in the mode line
  (if magit-wip-push-mode
      (advice-add 'magit-wip-commit-worktree
                  :around #'magit-wip-commit-worktree-around-advice-fn)
    (when (or t (advice--p #'magit-wip-commit-worktree))
      (advice-remove 'magit-wip-commit-worktree
                     #'magit-wip-commit-worktree-around-advice-fn))))

;;; misc-lib.el ends here
