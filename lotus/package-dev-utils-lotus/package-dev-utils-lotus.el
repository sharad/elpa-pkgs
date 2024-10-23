;;; package-dev-utils-lotus.el --- package-dev-utils-lotus         -*- lexical-binding: t; -*-

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap
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

;;; Code:

(provide 'package-dev-utils-lotus)


(require 'package)
(require 'package-x)
(require 'package-build "~/.emacs.d/core/libs/package-build.el")


(defvar package-source-path "~/.fa/src/elisp/elpa/elpa-pkgs/lotus" "Source code path for packages.")
(defvar package-archive-upload-base "~/.fa/src/elisp/elpa/upload")
(defvar package-local-dev-archive "local" "Local archive specified in package-archives")
(defvar *package-install-packages-wait-secs-in-install* 7)


(unless (assoc package-local-dev-archive package-archives)
  (push (cons package-local-dev-archive package-archive-upload-base)
        package-archives))


(unless (functionp 'directory-files-recursively)
  (defun directory-files-recursively (directory regexp &optional include-directories)
    "Return all files under DIRECTORY whose names match REGEXP.
This function searches the specified directory and its
sub-directories, recursively, for files whose basenames (i.e.,
without the leading directories) match the specified regexp, and
returns a list of the absolute file names of the matching
files (see absolute file names). The file names are returned in
depth-first order, meaning that files in some sub-directory are
returned before the files in its parent directory. In addition,
matching files found in each subdirectory are sorted
alphabetically by their basenames. By default, directories whose
names match regexp are omitted from the list, but if the optional
argument INCLUDE-DIRECTORIES is non-nil, they are included"
    (let* ((files-list '())
           (current-directory-list
            (directory-files directory t)))
      ;; while we are in the current directory
      (while current-directory-list
        (let ((f (cl-first current-directory-list)))
          (cond
           ((and (file-directory-p f)
                 (file-readable-p f)
                 (if include-directories (not (string-match regexp f)) t)
                 (not (string-equal ".." (substring f -2)))
                 (not (string-equal "." (substring f -1))))
            ;; recurse only if necessary
            (setq files-list (append files-list (directory-files-recursively f regexp include-directories))))
           ((and (file-regular-p f)
                 (file-readable-p f)
                 (string-match regexp f))
            (setq files-list (cons f files-list)))
           (t)))
        (setq current-directory-list (cl-rest current-directory-list)))
      files-list)))


(defun package-dev-build--valid-version (str &optional regexp)
  "Apply to STR the REGEXP if defined, \
then pass the string to `version-to-list' and return the result, \
or nil if the version cannot be parsed."
  (when (and regexp (string-match regexp str))
    (setq str (match-string 1 str)))
  (ignore-errors (version-to-list str)))


(defun package-install-local (pkg-desc)
  (let ((package-archives (list
                           (cons package-local-dev-archive package-archive-upload-base))))
    (message "Installing locally")
    (package-install pkg-desc)))

(defun package-make-package-desc (pkg-name &optional archive)
  (let* ((archive (or archive package-local-dev-archive))
         (package (assoc
                   (intern pkg-name)
                   (let* ((contents-file (format "archives/%s/archive-contents" archive))
                          (contents (package--read-archive-file contents-file)))
                     contents))))
    (if package
        (let* ((name (cl-first package))
               (version (package--ac-desc-version (cl-rest package)))
               (pkg-desc
                (package-desc-create
                 :name name
                 :version version
                 :reqs (package--ac-desc-reqs (cl-rest package))
                 :summary (package--ac-desc-summary (cl-rest package))
                 :kind (package--ac-desc-kind (cl-rest package))
                 :archive archive
                 :extras (and (> (length (cl-rest package)) 4)
                              ;; Older archive-contents files have only 4
                              ;; elements here.
                              (package--ac-desc-extras (cl-rest package))))))
          ;; (existing-packages (assq name package-archive-contents))
          ;; (pinned-to-archive (assoc name package-pinned-packages))

          pkg-desc)
      (error "not able to find package for %s" pkg-name))))

(defun package-desc-package-from-dir (dir &optional archive)
  (let* ((dir-of-current-file (directory-file-name dir))
         (pkg-name-version
          (file-name-nondirectory dir-of-current-file))
         (pkg-name
          (replace-regexp-in-string
           "-[0-9\.]\*\$" "" pkg-name-version))
         (version
          (package-version-join
           (package-dev-build--valid-version
            (format-time-string "%Y%m%d.%H%M")))))
    (package-make-package-desc pkg-name package-local-dev-archive)))

(defun package-requirements-package-from-dir (dir)
  (let* ((dir-of-current-file  (directory-file-name dir))
         (pkg-name-version     (file-name-nondirectory dir-of-current-file))
         (pkg-name             (replace-regexp-in-string "-[0-9\.]\*\$"
                                                         ""
                                                         pkg-name-version))
         (version              (package-version-join (package-dev-build--valid-version)
                                                     (format-time-string "%Y%m%d.%H%M")))
         (currdir-pkg-def-file (expand-file-name (format "%s-pkg.el" pkg-name)
                                                 dir-of-current-file))
         (pkg-def-exists       (file-exists-p currdir-pkg-def-file))
         (pkg-def              (let ((pkg-def-file currdir-pkg-def-file))
                                 (if (file-exists-p pkg-def-file)
                                     (car (read-from-string (with-temp-buffer
                                                              (insert-file-contents-literally pkg-def-file)
                                                              (let ((contents (condition-case e
                                                                                  ;; (read (current-buffer))
                                                                                  (buffer-string)
                                                                                ('end-of-file nil))))
                                                                contents))))
                                   `(define-package ,pkg-name ,version ,(format "%s" pkg-name) nil)))))
    (nth 1 (nth 4 pkg-def))))


;;;###autoload
(defun package-load-package-from-dir (dir)
  (message "loading file from %s" dir)
  (let ((default-directory dir))
    (add-to-list 'load-path dir)
    (dolist (file (directory-files dir t ".el$"))
      (unless (or (string-match "^.*-pkg.el$" file)
                  (string-match "^#.*-pkg.el$" file)
                  (string-match "^\..*-pkg.el$" file))
        (message "loading file %s" file)
        (load-file file)))))


;;;###autoload
(defun package-build-package-from-dir (dir &optional force update-source-pkg-desc)
  (interactive (let ((dir (read-directory-name "package directory: ")))
                 (message "package-build-package-from-dir: current-prefix-arg %s" current-prefix-arg)
                 (list dir current-prefix-arg)))
  ;; version is today date
  (let* ((dir-of-current-file  (directory-file-name dir))
         (pkg-name-version     (file-name-nondirectory dir-of-current-file))
         (pkg-name             (replace-regexp-in-string "-[0-9\.]\*\$" "" pkg-name-version))
         (version              (package-version-join (package-dev-build--valid-version
                                                      (format-time-string "%Y%m%d.%H%M"))))
         (currdir-pkg-def-file (expand-file-name (format "%s-pkg.el" pkg-name)
                                                 dir-of-current-file))
         (pkg-def-exists       (file-exists-p currdir-pkg-def-file))
         (pkg-def              (let ((pkg-def-file currdir-pkg-def-file))
                                 (if (file-exists-p pkg-def-file)
                                     (car (read-from-string
                                           (with-temp-buffer
                                             (insert-file-contents-literally pkg-def-file)
                                             (let ((contents
                                                    (condition-case e
                                                        ;; (read (current-buffer))
                                                        (buffer-string)
                                                      ('end-of-file nil))))
                                               contents))))
                                   `(define-package ,pkg-name ,version ,(format "%s" pkg-name) nil))))
         (tmp-dir              (expand-file-name "elpa" (or (getenv "TMP") "~/tmp/")))
         (pkg-dir              (expand-file-name (format "%s-%s" pkg-name version)
                                                 tmp-dir)))

    ;; (package-load-package-from-dir dir)
    (message "building package %s" pkg-name)
    (when (or (file-exists-p currdir-pkg-def-file)
              (or force
                  (y-or-n-p (format "Do you want to make package of %s from %s (force %s): "
                                    pkg-name
                                    dir-of-current-file
                                    force))))
      ;; add org-tangle-file here
      (let ((default-directory dir-of-current-file)
            ;; https://github.com/justinbarclay/parinfer-rust-mode/issues/52
            (parinfer-rust-check-before-enable nil))
        (dolist (org-file (directory-files dir-of-current-file t "'\*\.org$"))
          (let ((org-babel-tangle-file org-file))
            (org-babel-tangle-file org-file))))
      (copy-directory dir-of-current-file pkg-dir nil t t)
      (when current-prefix-arg
        (package-load-package-from-dir pkg-dir))
      ;; delete unnecessary files
      (let ((default-directory pkg-dir))
        (dolist (del-file (directory-files-recursively pkg-dir "'\*~$\\|'RCS$"))
          (delete-file del-file)))

      (when nil
        (if (file-directory-p package-source-path)
            (unless (string-match-p
                     (concat "^"
                             (regexp-quote
                              (file-truename package-source-path)))
                     (file-truename dir-of-current-file))
              (copy-directory dir-of-current-file
                              (expand-file-name pkg-name package-source-path)
                              nil t t))
          (error "package-source-path do ot exists.")))
      (setcar (nthcdr 2 pkg-def) version)
      (unless pkg-def-exists ;; version exist mean file -pkg.el was there presently, so need to ask any question.
        (progn
          (setcar (nthcdr 2 pkg-def) version)
          (unless (nth 3 pkg-def)
            (let ((desc
                   (read-from-minibuffer (format "package %s desc: " pkg-name))))
              (setcar (nthcdr 3 pkg-def) desc)))
          (when nil
            (unless (nth 4 pkg-def)
              (let ((dep
                     (cl-first (read-from-string (read-from-minibuffer (format "package %s dependency: " pkg-name))))))
                (setcar (nthcdr 4 pkg-def) dep))))))

      (let ((pkgdir-def-file (expand-file-name (format "%s-pkg.el" pkg-name) pkg-dir)))
        (with-current-buffer
            ;; NOTE: don't use find-file-noselect -- it will bring autoinsert even if find-file-hook made to be NIL.
            (or (find-buffer-visiting pkgdir-def-file)
                (find-file-literally pkgdir-def-file))
          (set-buffer-file-coding-system
           (if (coding-system-p 'utf-8-emacs)
               'utf-8-emacs
             'emacs-mule))
          (erase-buffer)
          (when (fboundp 'vhl/ext/delete/off)
            (vhl/ext/delete/off))
          (let ((content (let ((print-length nil)
                               (print-level nil))
                           (pp-to-string pkg-def))))
            ;; (message "TEST: %s" content)
            (insert content)
            (write-file pkgdir-def-file)))

        (when (or (not pkg-def-exists)
                  update-source-pkg-desc)
          (message "Creating %s file" currdir-pkg-def-file)
          ;; TODO: copy pkgdir-def-file dir-of-current-file/*-pkg.el
          (copy-file pkgdir-def-file currdir-pkg-def-file t))

        (let ((pkgdir-def-file-buff (find-buffer-visiting pkgdir-def-file)))
          (when pkgdir-def-file-buff (kill-buffer pkgdir-def-file-buff))))

      (let ((default-directory tmp-dir)
            (pkg-tar-file-name (if (shell-command
                                    (format "tar cf %s/%s-%s.tar -C %s %s-%s"
                                            tmp-dir pkg-name version
                                            tmp-dir
                                            pkg-name version))
                                   (format "%s/%s-%s.tar"
                                           tmp-dir pkg-name version))))
        (progn
          (message "built package %s" pkg-name)
          (package-load-package-from-dir dir)
          pkg-tar-file-name)))))

;;;###autoload
(defun package-upload-package-from-dir (dir &optional force archive)
  (interactive
   (let ((dir (read-directory-name "package directory: ")))
     (list dir current-prefix-arg)))
  (let* ((pkg-tar (package-build-package-from-dir dir force))
         (pkg-name
          (replace-regexp-in-string
           "-[0-9\.]\*\.tar\\(\.gz\\)?\$" ""
           (file-name-nondirectory pkg-tar)))
         (dst-pkg-tar (expand-file-name (file-name-nondirectory pkg-tar)
                                        package-archive-upload-base)))
    (message "uploading package %s" pkg-name)
    (when (string= (file-name-extension pkg-tar) "tar")
      (package-upload-file pkg-tar)
      (if (file-directory-p package-archive-upload-base)
          (progn
            (let ((old-pkgs (directory-files
                             package-archive-upload-base
                             t
                             (concat (regexp-quote pkg-name) "-[0-9\.]\+\.tar"))))
              (dolist (op old-pkgs)
                (delete-file op)))
            (message "copy %s %s"
                     pkg-tar
                     dst-pkg-tar)
            (copy-file pkg-tar
                       dst-pkg-tar
                       t))
        (error "package-archive-upload-base not exists."))
      (let ((package-archives (list
                               (cons package-local-dev-archive package-archive-upload-base))))
        (package-refresh-contents)))
    (message "uploaded package %s" pkg-name)
    (package-make-package-desc pkg-name (or archive package-local-dev-archive))))

;;;###autoload
(defun package-install-package-from-dir (dir &optional force)
  (interactive
   (let ((dir (read-directory-name "package directory: ")))
     (list dir current-prefix-arg)))
  (let* ((pkg-desc (package-upload-package-from-dir dir force))
         (pkg-sym  (package-desc-name pkg-desc))
         (pkg-name (symbol-name pkg-sym)))
    (when (package-installed-p pkg-sym)
      ;; (package-delete pkg-desc)
      (when pkg-desc
        (ignore-errors
          (package-delete pkg-desc))))

    (let ((old-installed-pkgs (directory-files
                               package-user-dir
                               t
                               (concat (regexp-quote pkg-name) "-[0-9\.]\+"))))
      (dolist (oipdir old-installed-pkgs)
        (delete-directory oipdir t)))
    (message "installing package %s" pkg-sym)
    (unless (package-install-local pkg-desc)
      (package-install pkg-desc))
    (message "installed package %s" pkg-sym)))

;;;###autoload
(defun package-build-packages-from-source-path (&optional base force)
  (interactive
   (list (read-directory-name "pacakages dir: ")
         current-prefix-arg))
  (let ((base (or base package-source-path)))
    (dolist (f (directory-files base))
      (let ((pkgdir (expand-file-name f base)))
        (when (and (file-directory-p pkgdir)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (package-build-package-from-dir pkgdir force))))))
;;;###autoload
(defun package-upload-packages-from-source-path (&optional base force)
  (interactive
   (list (read-directory-name "pacakages dir: ")
         current-prefix-arg))
  (let ((base (or base package-source-path)))
    (dolist (f (directory-files base))
      (let ((pkgdir (expand-file-name f base)))
        (when (and (file-directory-p pkgdir)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (package-upload-package-from-dir pkgdir force))))))
;;;###autoload
(defun package-install-packages-from-source-path (&optional base force)
  (interactive
   (list (read-directory-name "pacakages dir: ")
         current-prefix-arg))
  (let ((base (or base package-source-path)))
    (dolist (f (directory-files base))
      (let ((pkgdir (expand-file-name f base)))
        (sleep-for *package-install-packages-wait-secs-in-install*)
        (when (and (file-directory-p pkgdir)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (ignore-errors
            (package-install-package-from-dir pkgdir force))
          (message "Installed %s" pkgdir))))
    (message "Installed all packages from %s" base)))

;;;###autoload
(defun package-install-packages-from-source-path-fast (&optional base)
  (interactive
   (list (read-directory-name "pacakages dir: ")))
  (let ((base (or base package-source-path)))

    ;; First build and upload all subdirs
    (progn
      (package-upload-packages-from-source-path base)
      (message "Uploaded all packages from %s" base))

    (let* ((subdirs (remove-if-not #'(lambda (d) (file-directory-p (expand-file-name d base))) (remove ".." (remove "." (directory-files base)))))
           (subdir-paths (mapcar #'(lambda (d) (expand-file-name d base)) subdirs))
           (subdir-paths (remove-if-not #'file-directory-p subdir-paths))
           (dependencies-with-version
            (apply #'append
                   (remove nil
                           (mapcar #'package-requirements-package-from-dir
                                   subdir-paths))))
           (dependencies-without-version
            (delete-dups (mapcar #'car dependencies-with-version)))
           (dependencies-external
            (remove-if #'(lambda (d)
                           (file-directory-p
                            (expand-file-name (symbol-name d) base)))
                       dependencies-without-version)))
      ;; (dependencies-external
      ;;  (delete-dups
      ;;   dependencies-external))


      ;; Than find all dependencies and install them
      ;; try to install dependencies first
      (progn
        (message "depecdencies to be installed %s" dependencies-external)
        (dolist (dep dependencies-external)
          (let ((dep-sym  (cl-first (assoc dep package-archive-contents)))
                (dep-desc (nth 1 (assoc dep package-archive-contents))))
            (if (package-installed-p dep-sym)
                (message "dependency package %s already installed." dep)
              (progn
                (message "installing dep %s %s" dep (symbolp dep))
                (package-install dep-desc)
                (sleep-for *package-install-packages-wait-secs-in-install*)
                (message "Installed dependency package %s" dep)))))
        (message "dependencies installed %s" dependencies-external))

      ;; now install all subdirs.
      (progn
        (dolist (pkg-path subdir-paths)
          ;; as already uploaded.
          (let (;; (dep-desc (nth 1 (assoc dep package-archive-contents)))
                (pkg (package-desc-package-from-dir pkg-path)))
            (message "Installing %s" pkg-path)
            (package-install pkg)
            (sleep-for *package-install-packages-wait-secs-in-install*)
            (message "Installed %s" pkg-path)))
        (message "Installed all packages from %s" base)))))

(defun package-requirement-from-package (pkg) ;uploaded package
  (let ((pkg-desc (if (package-desc-p pkg) pkg (package-make-package-desc pkg))))
    (mapcar 'car '(package-desc-reqs pkg-desc))))


;; check about
;; package-compute-transaction

;;;###autoload
(defun lotus-package-delete (pkg)
  (interactive
   (progn
     ;; Initialize the package system to get the list of package
     ;; symbols for completion.
     (unless package--initialized
       (package-initialize t))
     (unless package-archive-contents
       (package-refresh-contents))
     (list (intern (completing-read
                    "Install package: "
                    (delq nil
                          (mapcar (lambda (elt)
                                    (when (package-installed-p (cl-first elt))
                                      (symbol-name (cl-first elt))))
                                  ;; package-archive-contents
                                  package-alist))
                    nil t)))))
  (add-hook 'post-command-hook #'package-menu--post-refresh)
  (let ((pkg-desc (if (package-desc-p pkg)
                      pkg
                    (nth 1 (assq pkg package-alist)))))
    (if pkg-desc
        (if (package-installed-p (package-desc-name pkg-desc))
            (package-delete pkg-desc t)
          (message "package %s is not installed" pkg))
      (message "No such package %s" pkg))))

;;;###autoload
(defun lotus-package-autoremove ()
  "Remove packages that are no more needed.

Packages that are no more needed by other packages in
`package-selected-packages' and their dependencies
will be deleted."
  (interactive)
  ;; If `package-selected-packages' is nil, it would make no sense to
  ;; try to populate it here, because then `package-autoremove' will
  ;; do absolutely nothing.
  (when (or package-selected-packages
            (yes-or-no-p
             (format-message
              "`package-selected-packages' is empty! Really remove ALL packages? ")))
    (let* ((removable
            (package--removable-packages))

           (removable
            (remove-if-not
             #'(lambda (p)
                 (let* ((pdesc (nth 1 (assq p package-alist)))
                        (dir (if pdesc
                                 (package-desc-dir pdesc))))
                   (string-prefix-p (file-name-as-directory
                                     (expand-file-name package-user-dir))
                                    (expand-file-name dir))))
             removable)))

      (if removable
          (when (y-or-n-p
                 (format "%s packages will be deleted:\n%s, proceed? "
                         (length removable)
                         (mapconcat #'symbol-name removable ", ")))
            (mapc (lambda (p)
                    (package-delete (nth 1
                                         (assq p package-alist)) t))
                  removable))
        (message "Nothing to autoremove")))))


;; check about
;; package-compute-transaction

(when nil
  (package-desc-p (cl-rest (cl-first package-archive-contents)))
  (cl-first (cl-first package-archive-contents))
  (assoc
   'yasnippet-classic-snippets
   package-archive-contents))

;;;###autoload
(defun package-resolve-org-plus-contrib-upgrade ()
  (interactive)
  (message "started package-resolve-org-plus-contrib-upgrade")
  (let ((pkgs '(ox-pandoc
                orgit
                org-ql
                org-randomnote
                org-present
                org-category-capture
                org-projectile
                org-brain
                ob-elixir
                ob-async
                elfeed-org
                org-password-manager
                org-clock-unnamed-task
                occ
                org
                org-plus-contrib)))

    (dolist (p pkgs)
      (ignore-errors (lotus-package-delete p)))

    (dolist (p (reverse pkgs))
      (package-install p)))
  (message "finished package-resolve-org-plus-contrib-upgrade"))

;;;###autoload
(defun package-install-locally-from-dir (dir)
  ;; https://emacs.stackexchange.com/a/7589
  (interactive
   (list (read-directory-name "pacakages dir: ")))
  (let* ((basename (file-name-nondirectory dir))
         (dirpath  (dirname-of-file dir))
         (tar-file (format "/tmp/%s" basename)))
    (when (shell-command (format "tar czf %s -C %s %s"
                                 tar-file
                                 dirpath
                                 basename))
      (when (package-install-file tar-file)
        (delete-file tar-file)))))

;;; package-dev-utils-lotus.el ends here
