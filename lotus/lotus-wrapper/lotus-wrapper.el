;;; lotus-wrapper.el --- rcs autosave backup  -*- lexical-binding: t; -*-

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sharad>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(provide 'lotus-wrapper)


(require 'caching-file-truename-advice)
(require 'lotus-misc-advices)
(require 'files-advices)
(require 'lsp-advices)


(declare-function around--org--newline "lotus-misc-advices" (oldfn &rest args))


;;;###autoload
(defun lotus-wrapper-insinuate ()
  (interactive)
  (with-eval-after-load "files"
    (add-function :override
                  (symbol-function 'file-truename)
                  #'override--file-truename)
    (advice-add 'dir-locals-find-file
                :around #'dir-locals-find-file-around-advice-fn-with-new-locate-dominating-file)
    (advice-add 'dir-locals-collect-variables :around
                #'dir-locals-collect-variables-around-advice-fn-with-file-truename))

  (with-eval-after-load "lsp-mode"
    (advice-add 'lsp-find-session-folder :around
                #'lsp-find-session-folder-around-advice-fn-with-file-truename))

  (with-eval-after-load "polymode-core"
    (add-function :override
                  (symbol-function 'pm--run-other-hooks)
                  #'override--pm--run-other-hooks))

  (with-eval-after-load "semantic"
    (add-function :around (symbol-function 'semantic-mode)
                  #'around--semantic-mode))

  (disable-file-truename-ad--set-advices "compile"
                                         '(compilation-find-file
                                           compilation-get-file-structure))

  (disable-file-truename-ad--set-advices "projectile"
                                         '(projectile-cache-current-file
                                           delete-file-projectile-remove-from-cache
                                           projectile-project-root
                                           projectile-project-buffer-p
                                           projectile-select-files
                                           projectile-compilation-dir))

  (disable-file-truename-ad--set-advices "lsp-mode"
                                         '(lsp--all-watchable-directories
                                           lsp-watch-root-folder
                                           lsp--server-register-capability
                                           lsp--folder-watch-callback))

  (disable-file-truename-ad--set-advices "lsp-headerline"
                                         '(lsp-headerline--build-path-up-to-project-string))

  (disable-file-truename-ad--set-advices "ggtags"
                                         '(ggtags-create-tags
                                           ggtags-find-project))
  (ignore-error
      (with-eval-after-load "erc-ident"
        (add-function :override
                      (symbol-function 'erc-identd-start)
                      #'override--erc-identd-start))))

;;;###autoload
(defun lotus-wrapper-uninsinuate ()
  (interactive)
  (remove-function (symbol-function 'file-truename)
                   #'override--file-truename)
  (advice-remove 'dir-locals-find-file
                 #'dir-locals-find-file-around-advice-fn-with-new-locate-dominating-file)
  (advice-remove 'dir-locals-collect-variables
                 #'dir-locals-collect-variables-around-advice-fn-with-file-truename)
  (advice-remove 'lsp-find-session-folder
                 #'lsp-find-session-folder-around-advice-fn-with-file-truename)
  (remove-function (symbol-function 'pm--run-other-hooks)
                   #'override--pm--run-other-hooks)
  (remove-function (symbol-function 'semantic-mode)
                   #'around--semantic-mode)
  (disable-file-truename-ad--unset-advices "compile"
                                           '(compilation-find-file
                                             compilation-get-file-structure))

  (disable-file-truename-ad--unset-advices "projectile"
                                           '(projectile-cache-current-file
                                             delete-file-projectile-remove-from-cache
                                             projectile-project-root
                                             projectile-project-buffer-p
                                             projectile-select-files
                                             projectile-compilation-dir))

  (disable-file-truename-ad--unset-advices "lsp-mode"
                                           '(
                                             lsp--all-watchable-directories
                                             lsp-watch-root-folder
                                             lsp--server-register-capability
                                             lsp--folder-watch-callback
                                             ))

  (disable-file-truename-ad--unset-advices "lsp-headerline"
                                           '(lsp-headerline--build-path-up-to-project-string))

  (disable-file-truename-ad--unset-advices "ggtags"
                                           '(ggtags-create-tags
                                             ggtags-find-project))
  (ignore-error
      (remove-function (symbol-function 'erc-identd-start)
                       #'override--erc-identd-start)))

;;; lotus-wrapper.el ends here






(defun compilation-find-file (marker filename directory &rest formats)
  "Find a buffer for file FILENAME.
If FILENAME is not found at all, ask the user where to find it.
Pop up the buffer containing MARKER and scroll to MARKER if we ask
the user where to find the file.
Search the directories in `compilation-search-path'.
A nil in `compilation-search-path' means to try the
\"current\" directory, which is passed in DIRECTORY.
If DIRECTORY is relative, it is combined with `default-directory'.
If DIRECTORY is nil, that means use `default-directory'.
FORMATS, if given, is a list of formats to reformat FILENAME when
looking for it: for each element FMT in FORMATS, this function
attempts to find a file whose name is produced by (format FMT FILENAME)."
  (or formats (setq formats '("%s")))
  (let ((dirs compilation-search-path)
        (spec-dir (if directory
                      (expand-file-name directory)
                    default-directory))
        buffer thisdir fmts name)
    (if (and filename
             (file-name-absolute-p filename))
        ;; The file name is absolute.  Use its explicit directory as
        ;; the first in the search path, and strip it from FILENAME.
        (setq filename (abbreviate-file-name (expand-file-name filename))
              dirs (cons (file-name-directory filename) dirs)
              filename (file-name-nondirectory filename)))
    ;; Now search the path.
    (while (and dirs (null buffer))
      (setq thisdir (or (car dirs) spec-dir)
            fmts formats)
      ;; For each directory, try each format string.
      (while (and fmts (null buffer))
        (setq name (identity
                    (file-name-concat thisdir (format (car fmts) filename)))
              buffer (and (file-exists-p name)
                          (find-file-noselect name))
              fmts (cdr fmts)))
      (setq dirs (cdr dirs)))
    ;; If we haven't found it, this might be a parallel build.
    ;; Search the directories further up the buffer.
    (when (and (null buffer)
               compilation-search-all-directories)
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char (marker-position marker))
          (when-let ((prev (compilation--previous-directory (point))))
            (goto-char prev))
          (setq dirs (cdr (or (get-text-property
                               (1- (point)) 'compilation-directory)
                              (get-text-property
                               (point) 'compilation-directory))))))
      (while (and dirs (null buffer))
        (setq thisdir (car dirs)
              fmts formats)
        (while (and fmts (null buffer))
          (setq name (identity
                      (file-name-concat thisdir (format (car fmts) filename)))
                buffer (and (file-exists-p name)
                            (find-file-noselect name))
                fmts (cdr fmts)))
        (setq dirs (cdr dirs))))
    (while (null buffer)    ;Repeat until the user selects an existing file.
      ;; The file doesn't exist.  Ask the user where to find it.
      (save-excursion            ;This save-excursion is probably not right.
        (let ((w (let ((pop-up-windows t))
                   (display-buffer (marker-buffer marker)
                                   '(nil (allow-no-window . t))))))
          (with-current-buffer (marker-buffer marker)
            (goto-char marker)
            (and w (progn (compilation-set-window w marker)
                          (compilation-set-overlay-arrow w))))
          (let* ((name (read-file-name
                        (format-prompt "Find this %s in"
                                       filename compilation-error)
                        spec-dir filename t nil
                        ;; The predicate below is fine when called from
                        ;; minibuffer-complete-and-exit, but it's too
                        ;; restrictive otherwise, since it also prevents the
                        ;; user from completing "fo" to "foo/" when she
                        ;; wants to enter "foo/bar".
                        ;;
                        ;; Try to make sure the user can only select
                        ;; a valid answer.  This predicate may be ignored,
                        ;; tho, so we still have to double-check afterwards.
                        ;; TODO: We should probably fix read-file-name so
                        ;; that it never ignores this predicate, even when
                        ;; using popup dialog boxes.
                        ;; (lambda (name)
                        ;;   (if (file-directory-p name)
                        ;;       (setq name (expand-file-name filename name)))
                        ;;   (file-exists-p name))
                        ))
                 (origname name))
            (cond
             ((not (file-exists-p name))
              (message "Cannot find file `%s'" name)
              (ding) (sit-for 2))
             ((and (file-directory-p name)
                   (not (file-exists-p
                         (setq name (identity
                                     (file-name-concat name filename))))))
              (message "No `%s' in directory %s" filename origname)
              (ding) (sit-for 2))
             (t
              (setq buffer (find-file-noselect name))))))))
    ;; Make intangible overlays tangible.
    ;; This is weird: it's not even clear which is the current buffer,
    ;; so the code below can't be expected to DTRT here.  -- Stef
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'intangible)
        (overlay-put ov 'intangible nil)))
    buffer))


(defun ggtags-find-project ()
  ;; See https://github.com/leoliu/ggtags/issues/42
  ;;
  ;; It is unsafe to cache `ggtags-project-root' in non-file buffers
  ;; whose `default-directory' can often change.
  (unless (equal ggtags-last-default-directory default-directory)
    (kill-local-variable 'ggtags-project-root))
  (let ((project (gethash ggtags-project-root ggtags-projects)))
    (if (ggtags-project-p project)
        (if (ggtags-project-expired-p project)
            (progn
              (remhash ggtags-project-root ggtags-projects)
              (ggtags-find-project))
          project)
      (setq ggtags-last-default-directory default-directory)
      (setq ggtags-project-root
            (or (ignore-errors
                  (file-name-as-directory
                   (concat (file-remote-p default-directory)
                           ;; Resolves symbolic links
                           ;; (ggtags-process-string "global" "-pr")
                           (expand-file-name (file-relative-name (ggtags-process-string "global" "-pr") (file-truename default-directory))
                                             default-directory))))
                ;; 'global -pr' resolves symlinks before checking the
                ;; GTAGS file which could cause issues such as
                ;; https://github.com/leoliu/ggtags/issues/22, so
                ;; let's help it out.
                (let ((dir (locate-dominating-file
                            default-directory
                            (lambda (dir) (file-regular-p (expand-file-name "GTAGS" dir))))))
                  ;; `file-truename' may strip the trailing '/' on
                  ;; remote hosts, see http://debbugs.gnu.org/16851
                  (and dir (file-name-as-directory
                            ;; (file-truename dir)
                            (identity dir)
                            )))))
      (when ggtags-project-root
        (if (gethash ggtags-project-root ggtags-projects)
            (ggtags-find-project)
          (ggtags-make-project ggtags-project-root))))))


(defun lsp-headerline--build-path-up-to-project-string ()
  "Build the path-up-to-project segment for the breadcrumb."
  (if-let ((root (lsp-headerline--workspace-root)))
      (let ((segments (progn
                        (unless lsp-headerline--path-up-to-project-segments
                          (setq lsp-headerline--path-up-to-project-segments
                                (list (lsp-headerline--path-up-to-project-root
                                       root
                                       (file-name-directory (buffer-file-name))))))
                        (car lsp-headerline--path-up-to-project-segments))))
        (mapconcat (lambda (next-dir)
                     (propertize next-dir
                                 'font-lock-face
                                 (lsp-headerline--face-for-path
                                  (get-text-property
                                   0 'lsp-full-path next-dir))))
                   segments
                   (concat " " (lsp-headerline--arrow-icon) " ")))
    ""))

