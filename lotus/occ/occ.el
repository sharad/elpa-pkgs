;;; occ.el --- occ               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  sharad

;; Author: sharad <>
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

;; https://economictimes.indiatimes.com/small-biz/hr-leadership/leadership/getting-more-done-why-task-management-is-the-key-to-managing-time/articleshow/70814450.cms

;;; Code:

(provide 'occ)


(require 'switch-buffer-functions)


(require 'occ-main)
(require 'occ-util-common)
(require 'occ-commands)
(require 'occ-resolve-clock)
(require 'occ-test)
(require 'occ-config)


(defvar *occ-collector* nil)
(defvar *occ-collector-default-key* "default")

(defun occ-collector-get (key)
  (rest (assoc key *occ-collector*)))
(defun occ-collector-set (key value)
  (setcdr (assoc key *occ-collector*) value))
(defun occ-collector-spec (key)
  (let ((collection (occ-collector-get key)))
    (when collection
      (occ-obj-collection-spec collection))))
(defun occ-collector-roots (key)
  (let ((collection (occ-collector-get key)))
    (when collection
      (occ-obj-collection-roots collection))))
(defun occ-collector-files (key)
  (let ((collection (occ-collector-get key)))
    (when collection
      (occ-obj-collection-files collection))))

;; ;;;###autoload
;; (defun occ-switch-buffer-run-curr-ctx-timer-function (prev next)
;;   (occ-run-curr-ctx-timer))

;;;###autoload
(defun occ-add-after-save-hook-fun-in-org-mode ()
  (add-hook 'after-save-hook 'occ-after-save-hook-fun t t))


;;;###autoload
(defun occ-set-global-tsk-collection-spec (spec)
  (setq occ-global-tsk-collection      nil
        occ-global-tsk-collection-spec spec))

(defun occ-reset-global-tsk-collection ()
  (occ-debug "resetting global-tsk-collection")
  (occ-reset-collection-object))


;;;###autoload
(defun occ-initialize (&optional spec)
  "occ-initialize"
  (setq *occ-tsk-previous-ctx* (occ-obj-make-ctx-at-point))
  (progn
    (setq occ-mode t)
    (occ-enable-mode-map)
    (occ-register-resolve-clock)
    (occ-cancel-timer)
    (occ-reset-collection-object)
    (occ-ctx-clrhash)
    ;; (add-hook 'buffer-list-update-hook     'occ-run-curr-ctx-timer t)
    ;; (add-hook 'elscreen-screen-update-hook 'occ-run-curr-ctx-timer t)
    ;; (add-hook 'elscreen-goto-hook          'occ-run-curr-ctx-timer t)
    (add-hook 'switch-buffer-functions #'occ-switch-buffer-run-curr-ctx-timer-function)
    (add-hook 'org-mode-hook           #'occ-add-after-save-hook-fun-in-org-mode)
    (add-hook 'org-mode-hook           #'occ-add-org-file-timer))
  (dolist (prop (occ-obj-properties-to-inherit nil))
    (let ((propstr (upcase (if (keywordp prop)
                               (substring (symbol-name prop) 1)
                             (symbol-name prop)))))
      (unless (member propstr org-use-property-inheritance)
        (push propstr org-use-property-inheritance))))
  (progn
    (unless occ-global-tsk-collection-spec
      (if (occ-valid-spec-p spec)
          (setq occ-global-tsk-collection-spec spec)
        (when (called-interactively-p 'interactive)
          (occ-obj-build-spec)))))
  ;; newly added
  (org-clock-load))

;;;###autoload
(defun occ-uninitialize ()
  "occ-uninitialize"
  (progn
    (setq occ-mode nil)
    (occ-disable-mode-map)
    (occ-unregister-resolve-clock)
    (occ-cancel-timer)
    (occ-reset-collection-object)
    (occ-ctx-clrhash)
    ;; (setq buffer-list-update-hook nil)

    ;; (remove-hook 'buffer-list-update-hook     'occ-run-curr-ctx-timer)
    ;; (remove-hook 'elscreen-screen-update-hook 'occ-run-curr-ctx-timer)
    ;; (remove-hook 'elscreen-goto-hook          'occ-run-curr-ctx-timer)
    ;; (remove-hook 'after-save-hook             'occ-after-save-hook-fun t)
    (remove-hook 'switch-buffer-functions #'occ-switch-buffer-run-curr-ctx-timer-function)
    (remove-hook 'org-mode-hook           #'occ-add-after-save-hook-fun-in-org-mode)
    (remove-hook 'org-mode-hook           #'occ-add-org-file-timer))
  (dolist (prop (occ-obj-properties-to-inherit nil))
    (let ((propstr
           (upcase (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))))
      (unless (member propstr org-use-property-inheritance)
        (delete propstr org-use-property-inheritance)))))


(defun occ-find-library-dir (library)
  (progn
     (delete* (expand-file-name occ-dev-dir library) load-path)
     (push (concat occ-dev-dir library) load-path)
     (let ((libpath (expand-file-name (concat library ".el")
                                      occ-dev-dir)))
       (if (file-exists-p libpath)
           occ-dev-dir
         (file-name-directory (or (locate-library library)
                                  "~/.xemacs/elpa/pkgs/occ/occ.el"
                                  ""))))))

(defun occ-get-version (here full message)
  "Show the Occ version.
Interactively, or when MESSAGE is non-nil, show it in echo area.
With prefix argument, or when HERE is non-nil, insert it at point.
In non-interactive uses, a reduced version string is output unless
FULL is given."
  (let ((occ-dir            (ignore-errors (occ-find-library-dir "occ")))
        (save-load-suffixes (when (boundp 'load-suffixes) load-suffixes))
        (load-suffixes      (list ".el"))
        (occ-install-dir    (ignore-errors (occ-find-library-dir "occ-loaddefs"))))
    (unless (and (fboundp 'occ-release)
                 (fboundp 'occ-git-version))
      (org-load-noerror-mustsuffix (concat occ-dir "occ-version")))
    (let* ((load-suffixes save-load-suffixes)
           (release       (occ-release))
           (git-version   (occ-git-version))
           (version (format "Occ mode version %s (%s @ %s)"
                            release
                            git-version
                            (if occ-install-dir
                                (if (string= occ-dir occ-install-dir)
                                    occ-install-dir
                                  (concat "mixed installation! "
                                          occ-install-dir
                                          " and "
                                          occ-dir))
                              "occ-loaddefs.el can not be found!")))
           (version1 (if full version release)))
      (when here (insert version1))
      ;; (when message (occ-message "%s" version1))
      version1)))

;;;###autoload
(defun occ-set-dev-dir (dirpath)
  (setq occ-dev-dir dirpath))

;;;###autoload
(defun occ-add-deps-libs (pkg)
  (let ((deps (cons (symbol-name pkg) (mapcar #'(lambda (x) (symbol-name (first x))) (package-desc-reqs (nth 1 (assoc 'occ package-alist)))))))
    (if occ-dev-dir
        (dolist (lib deps)
          (delete* (concat occ-dev-dir lib) load-path)
          (push (concat occ-dev-dir   lib) load-path))
      (occ-error "occ-dev-dir not defined"))))

;;;###autoload
(defun occ-load-pkg (pkg-str)
  ;; TODO: load all files in lib dir
  (let ((pkg-dir (occ-find-library-dir pkg-str)))
    (dolist (ef (directory-files pkg-dir nil ".el$"))
      (let ((efile (expand-file-name ef pkg-dir)))
        (unless (string-match "pkg.el$" efile)
          (occ-message "trying to load %s %s %s"
                       pkg-str
                       ef
                       efile)
          (or (and (occ-load-noerror-mustsuffix efile)
                   't)
              pkg-str))))))

;;;###autoload
(defun occ-reload-lib (uncompiled)
  "Reload all Occ Lisp files.
With prefix arg UNCOMPILED, load the uncompiled versions."
  (require 'loadhist)
  (let* ((pkg            'occ)
         (occ-dir        (occ-find-library-dir (symbol-name pkg)))
         ;; (contrib-dir (or (occ-find-library-dir "org-contribdir") occ-dir))
         ;; (feature-re "^\\(org\\|ob\\|ox\\)\\(-.*\\)?")
         (occ-deps       (cons (symbol-name pkg)
                               (mapcar #'(lambda (x) (symbol-name (first x)))
                                       (package-desc-reqs (nth 1 (assoc 'occ package-alist))))))
         (occ-pkg-regexp (regexp-opt occ-deps))
         (feature-re     (concat "^" occ-pkg-regexp "$"))
         (remove-re      (format "\\`%s\\'"
                                 (regexp-opt '("dash" "org" "org-loaddefs" "occ-version" "helm"))))
         (feats          (delete-dups (mapcar #'file-name-sans-extension
                                              (mapcar 'file-name-nondirectory
                                                      (delq nil (mapcar 'feature-file features))))))
         (lfeat          (append (sort (setq feats
                                             (delq nil (mapcar (lambda (f)
                                                                 (if (and (string-match feature-re f)
                                                                          (not (string-match remove-re f)))
                                                                     (progn
                                                                       (occ-message "%s matched." f)
                                                                       f)
                                                                   nil))
                                                               feats)))
                                       'string-lessp)
                          (list "occ-version" "occ")))
         (load-suffixes  (when (boundp 'load-suffixes) load-suffixes))
         (load-suffixes  (if uncompiled (reverse load-suffixes) load-suffixes))
         (load-uncore    nil)
         (load-misses    nil))
    (occ-add-deps-libs pkg)
    (occ-message "working on %s" lfeat)
    (let ((load-missed-1 (mapcar #'occ-load-pkg
                                 lfeat)))
     (setq load-misses (delq 't load-missed-1)))
    (occ-message "starting")
    (when load-uncore
      (occ-message "The following feature%s found in load-path, please check if that's correct:\n%s"
               (if (> (length load-uncore) 1) "s were" " was") load-uncore))
    (if load-misses
        (occ-message "Some error occurred while reloading Org feature%s\n%s\nPlease check *Messages*!\n%s"
                     (if (> (length load-misses) 1) "s" "")
                     load-misses (occ-version nil 'full))
      (occ-message "Successfully reloaded Org\n%s" (occ-version nil 'full)))))

(when nil
  (let* ((occ-pkg-regexp (regexp-opt (cons "occ" (mapcar #'(lambda (x) (symbol-name (first x))) (package-desc-reqs (nth 1 (assoc 'occ package-alist)))))))
         (feature-re     (concat "^" occ-pkg-regexp "$")))
    (string-match feature-re "xocc")))

;;; occ.el ends here
