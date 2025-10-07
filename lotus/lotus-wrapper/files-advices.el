;;; files-advices.el --- Files advice                -*- lexical-binding: t; -*-

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

(provide 'files-advices)


;;;###autoload
(defun locate-dominating--dirs-with-file-truename (filename name &optional counter prev-dirs trgdirs)
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

  (or counter (setq counter (list 350)))
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
                     (file-trgdir (locate-dominating--dirs-with-file-truename dirfile name counter prev-dirs trgdirs))
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
;;;###autoload
(defun locate-dominating--dirs-by-file (filename name)
  (cdr (locate-dominating--dirs-with-file-truename filename
                                                   name)))


(defun locate-dominating--nth-dir-by-file (filename n name)
  (nth n
       (locate-dominating--dirs-by-file filename name)))

(defun locate-dominating--first-dir-by-file (filename name)
  (locate-dominating--nth-dir-by-file filename 0 name))

(defalias 'locate-dominating--dir-by-file #'locate-dominating--first-dir-by-file)


(defun locate-dominating-nth-dir-by-file (filename n name)
  (let ((dir (nth n
                  (locate-dominating--dirs-by-file filename name))))
    (when dir (locate-dominating-file dir name))))

(defun locate-dominating-first-dir-by-file (filename name)
  (locate-dominating-nth-dir-by-file filename 0 name))

(defalias 'locate-dominating-dir-by-file #'locate-dominating-first-dir-by-file)



(when nil
  (locate-dominating-dir-by-file "~/.zshrc" ".git")

  (locate-dominating--file (file-name-directory file)
                           #'dir-locals--all-files)

  (dir-locals-find-file "~/.bin/selsecret")
  (dir-locals-find-file "~/.zshrc")

  (dir-locals-find-file (buffer-file-name))

  (locate-dominating-dir-by-file (buffer-file-name) ".dir-locals.el")

  (locate-dominating-dir-by-file "~/.zshrc" ".dir-locals.el")
  (locate-dominating-dir-by-file "~/.zshrc" #'dir-locals--all-files)
  (locate-dominating-dir-by-file "~/.zshrc" #'dir-locals--all-files))



;;;###autoload
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
                            (substring (file-truename file-name) ; <------------
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

;; (dir-locals-find-file "~/.zshrc")
;; (dir-locals-find-file (or (buffer-file-name) default-directory))
;; (dir-locals-read-from-dir dir-or-cache)

;; (dir-locals-read-from-dir (dir-locals-find-file "~/.zshrc"))


;;;###autoload
(defun around--dir-locals-find-file-around-advice-fn-with-new-locate-dominating-file (orgfn &rest args)
  (or (apply orgfn args)
      (apply #'locate-dominating-dir-by-file
             (append args
                     (list #'dir-locals--all-files)))))
;;;###autoload
(defun around--dir-locals-collect-variables-around-advice-fn-with-file-truename (orgfn &rest args)
  (condition-case err
      (apply orgfn args)
    (args-out-of-range
     (let ((file-name (cadr err)))
       (if (string= file-name (or (buffer-file-name)
                                  (expand-file-name default-directory)))
           (apply #'dir-locals-collect-variables-fn-with-file-truename
                  args)
         (error err))))))


;;;###autoload
(defun around--file-newer-than-file-p-length-fix (oldfn &rest r)
  (let ((file1 (car r))
        (file2 (cadr r)))
    (let ((name1 (file-name-nondirectory file1))
          (name2 (file-name-nondirectory file2)))
      (if (and (< (length name1) 255)
               (< (length name2) 255))
          (apply oldfn r)
        nil))))

;;; files-advices.el ends here
