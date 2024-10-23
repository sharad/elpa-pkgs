;;; caching-file-truename-advice.el --- cachine file-truename  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sharad

;; Author: s <>
;; Keywords: convenience, abbrev, abbrev, abbrev

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

(provide 'caching-file-truename-advice)


(defvar caching--file-truename-link-cycle-counter 300)

(defvar file-truename-do-caching t)

(defvar file-truename-cache nil)

(defvar file-truename-cache-dependency-list nil)

(defun enable-file-truename-cache ()
  (interactive)
  (setq file-truename-do-caching t))

(defun disable-file-truename-cache ()
  (interactive)
  (setq file-truename-do-caching nil))

(defun toggle-file-truename-cache ()
  (interactive)
  (setq file-truename-do-caching (not file-truename-do-caching)))

(defun caching--file-truename (filename &optional counter prev-dirs)
  "Return the truename of FILENAME.
If FILENAME is not absolute, first expands it against `default-directory'.
The truename of a file name is found by chasing symbolic links
both at the level of the file and at the level of the directories
containing it, until no links are left at any level.

\(fn FILENAME)"  ;; Don't document the optional arguments.
  ;; COUNTER and PREV-DIRS are only used in recursive calls.
  ;; COUNTER can be a cons cell whose car is the count of how many
  ;; more links to chase before getting an error.
  ;; PREV-DIRS can be a cons cell whose car is an alist
  ;; of truenames we've just recently computed.
  (let* ((original-filename filename)
         (found-filename (and file-truename-do-caching
                              (cl-rest (assoc original-filename
                                              file-truename-cache)))))

    ;; (when file-truename-do-caching
    ;;   (message "filename %s by cache: %s"
    ;;            original-filename
    ;;            (cl-rest (assoc original-filename file-truename-cache))))

    (if found-filename
        found-filename
      (progn
        (cond ((or (string= filename "") (string= filename "~"))
               (setq filename (expand-file-name filename))
               (if (string= filename "")
                   (setq filename "/")))
              ((and (string= (substring filename 0 1)
                             "~")
                    (string-match "~[^/]*/?"
                                  filename))
               (let ((first-part (substring filename
                                            0
                                            (match-end 0)))
                     (rest-part  (substring filename (match-end 0))))
                 (setq filename (concat (expand-file-name first-part) rest-part)))))

        (or counter
            (setq counter (list caching--file-truename-link-cycle-counter)))
        (let (done
              ;; For speed, remove the ange-ftp completion handler from the list.
              ;; We know it's not needed here.
              ;; For even more speed, do this only on the outermost call.
              (file-name-handler-alist
               (if prev-dirs file-name-handler-alist
                 (let ((tem (copy-sequence file-name-handler-alist)))
                   (delq (rassq 'ange-ftp-completion-hook-function tem) tem)))))
          (or prev-dirs (setq prev-dirs (list nil)))

          ;; andrewi@harlequin.co.uk - on Windows, there is an issue with
          ;; case differences being ignored by the OS, and short "8.3 DOS"
          ;; name aliases existing for all files.  (The short names are not
          ;; reported by directory-files, but can be used to refer to files.)
          ;; It seems appropriate for file-truename to resolve these issues in
          ;; the most natural way, which on Windows is to call the function
          ;; `w32-long-file-name' - this returns the exact name of a file as
          ;; it is stored on disk (expanding short name aliases with the full
          ;; name in the process).
          (if (eq system-type 'windows-nt)
              (unless (string-match "[[*?]" filename)
                ;; If filename exists, use its long name.  If it doesn't
                ;; exist, the recursion below on the directory of filename
                ;; will drill down until we find a directory that exists,
                ;; and use the long name of that, with the extra
                ;; non-existent path components concatenated.
                (let ((longname (w32-long-file-name filename)))
                  (if longname
                      (setq filename longname)))))

          ;; If this file directly leads to a link, process that iteratively
          ;; so that we don't use lots of stack.
          (while (not done)
            (setcar counter (1- (cl-first counter)))
            (if (< (cl-first counter) 0)
                (error "Apparent cycle of symbolic links for %s" filename))
            (let ((handler (find-file-name-handler filename 'file-truename)))
              ;; For file name that has a special handler, call handler.
              ;; This is so that ange-ftp can save time by doing a no-op.
              (if handler
                  (setq filename (funcall handler 'file-truename filename)
                        done t)
                (let ((dir (or (file-name-directory filename) default-directory))
                      target dirfile)
                  ;; Get the truename of the directory.
                  (setq dirfile (directory-file-name dir))
                  ;; If these are equal, we have the (or a) root directory.
                  (or (string= dir dirfile)
                      (and (memq system-type '(windows-nt ms-dos cygwin nacl))
                           (eq (compare-strings dir 0 nil dirfile 0 nil t) t))
                      ;; If this is the same dir we last got the truename for,
                      ;; save time--don't recalculate.
                      (if (assoc dir (cl-first prev-dirs))
                          (setq dir (cl-rest (assoc dir (cl-first prev-dirs))))
                        (let ((old dir)
                              (new (file-name-as-directory (file-truename dirfile counter prev-dirs))))
                          (setcar prev-dirs (cons (cons old new) (cl-first prev-dirs)))
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
                          (setq filename
                                (if (file-name-absolute-p target)
                                    target
                                  (concat dir target))
                                done nil)
                        ;; No, we are done!
                        (setq done t))))))))

          (when file-truename-do-caching

            (dolist (dirpair (cl-first prev-dirs))
              (let ((dir (cl-first dirpair)))
                (if (assoc dir file-truename-cache-dependency-list)
                    (unless (member
                             original-filename
                             (cl-rest (assoc dir file-truename-cache-dependency-list)))
                      (push original-filename (cdr (assoc dir file-truename-cache-dependency-list))))
                  (push (list dir original-filename) file-truename-cache-dependency-list))))

            (if (assoc original-filename file-truename-cache)
                (setcdr (assoc original-filename file-truename-cache) filename)
              (push (cons original-filename filename) file-truename-cache)))
          filename)))))
;;;###autoload
(defalias 'override--file-truename #'caching--file-truename)




(when nil
  (setq file-truename-do-caching nil)
  (setq file-truename-do-caching t)
  (caching--file-truename "~/.mailbox")
  (caching--file-truename "/home/s/hell/.fa/rc")


  (setq file-truename-do-caching nil)
  (setq file-truename-cache nil)
  (setq file-truename-cache-dependency-list nil)

  (cl-first (cl-first file-truename-cache-dependency-list)))

;; (caching--file-truename "~/.mailbox")

;; (caching--file-truename "/home/s/hell/.fa/rc")

;; (caching--file-truename
;;  "/home/s/hell/.setup"
;;  (181)
;;  (
;;   (
;;    ("/home/s/hell/.repos/git/main/resource/userorg/main/readwrite/public/user/" . "/home/s/hell/.repos/git/main/resource/userorg/main/readwrite/public/user/")
;;    ("/home/s/hell/.repos/git/main/resource/userorg/main/readwrite/public/" . "/home/s/hell/.repos/git/main/resource/userorg/main/readwrite/public/")
;;    ("/home/s/hell/.repos/git/main/resource/userorg/main/readwrite/" . "/home/s/hell/.repos/git/main/resource/userorg/main/readwrite/")
;;    ("/home/s/hell/.repos/git/main/resource/userorg/main/" . "/home/s/hell/.repos/git/main/resource/userorg/main/")
;;    ("/home/s/hell/.repos/git/main/resource/userorg/" . "/home/s/hell/.repos/git/main/resource/userorg/")
;;    ("/home/s/hell/.repos/git/main/resource/" . "/home/s/hell/.repos/git/main/resource/")
;;    ("/home/s/hell/.repos/git/main/" . "/home/s/hell/.repos/git/main/")
;;    ("/home/s/hell/.repos/git/" . "/home/s/hell/.repos/git/")
;;    ("/home/s/hell/.repos/" . "/home/s/hell/.repos/")
;;    ("/home/s/hell/" . "/home/s/hell/")
;;    ("/home/s/" . "/home/s/")
;;    ("/home/" . "/home/"))))


;; (define-minor-mode semantic-mode
;;   "Toggle parser features (Semantic mode).

;; In Semantic mode, Emacs parses the buffers you visit for their
;; semantic content.  This information is used by a variety of
;; auxiliary minor modes, listed in `semantic-default-submodes';
;; all the minor modes in this list are also enabled when you enable
;; Semantic mode.

;; \\{semantic-mode-map}"
;;   :global t
;;   :group 'semantic
;;   (if semantic-mode
;;       ;; Turn on Semantic mode
;;       (progn
;; 	;; Enable all the global auxiliary minor modes in
;; 	;; `semantic-submode-list'.
;; 	(dolist (mode semantic-submode-list)
;; 	  (and (memq mode semantic-default-submodes)
;; 	       (fboundp mode)
;; 	       (funcall mode 1)))
;; 	(unless semantic-load-system-cache-loaded
;; 	  (setq semantic-load-system-cache-loaded t)
;; 	  (when (and (boundp 'semanticdb-default-system-save-directory)
;; 		     (stringp semanticdb-default-system-save-directory)
;; 		     (file-exists-p semanticdb-default-system-save-directory))
;; 	    (require 'semantic/db-ebrowse)
;; 	    (semanticdb-load-ebrowse-caches)))
;; 	(add-hook 'mode-local-init-hook 'semantic-new-buffer-fcn)
;; 	;; Add semantic-ia-complete-symbol to
;; 	;; completion-at-point-functions, so that it is run from
;; 	;; M-TAB.
;; 	;;
;; 	;; Note: The first entry added is the last entry run, so the
;; 	;;       most specific entry should be last.
;; 	(add-hook 'completion-at-point-functions
;; 		  'semantic-analyze-nolongprefix-completion-at-point-function)
;; 	(add-hook 'completion-at-point-functions
;; 		  'semantic-analyze-notc-completion-at-point-function)
;; 	(add-hook 'completion-at-point-functions
;; 		  'semantic-analyze-completion-at-point-function)

;; 	(if (bound-and-true-p global-ede-mode)
;; 	    (define-key cedet-menu-map [cedet-menu-separator] '("--")))
;; 	(dolist (b (buffer-list))
;; 	  (when (buffer-live-p b)
;;       (with-current-buffer b
;; 	      (semantic-new-buffer-fcn)))))
;;     ;; Disable Semantic features.  Removing everything Semantic has
;;     ;; introduced in the buffer is pretty much futile, but we have to
;;     ;; clean the hooks and delete Semantic-related overlays, so that
;;     ;; Semantic can be re-activated cleanly.
;;     (remove-hook 'mode-local-init-hook 'semantic-new-buffer-fcn)
;;     (remove-hook 'completion-at-point-functions
;; 		 'semantic-analyze-completion-at-point-function)
;;     (remove-hook 'completion-at-point-functions
;; 		 'semantic-analyze-notc-completion-at-point-function)
;;     (remove-hook 'completion-at-point-functions
;; 		 'semantic-analyze-nolongprefix-completion-at-point-function)

;;     (remove-hook 'after-change-functions
;; 		 'semantic-change-function)
;;     (define-key cedet-menu-map [cedet-menu-separator] nil)
;;     (define-key cedet-menu-map [semantic-options-separator] nil)
;;     ;; FIXME: handle semanticdb-load-ebrowse-caches
;;     (dolist (mode semantic-submode-list)
;;       (if (and (boundp mode) (eval mode))
;; 	  (funcall mode -1)))
;;     ;; Unlink buffer and clear cache
;;     (semantic--tag-unlink-cache-from-buffer)
;;     (setq semantic--buffer-cache nil)
;;     ;; Make sure we run the setup function if Semantic gets
;;     ;; re-activated.
;;     (setq semantic-new-buffer-fcn-was-run nil)))

;;; caching-file-truename-advice.el ends here
