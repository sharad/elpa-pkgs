;;; lotus-wrapper.el --- rcs autosave backup

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
(require 'disable-file-truname-advices)
(require 'lotus-misc-advices)


;;;###autoload
(defun lotus-wrapper-insinuate ()
  (interactive)
  (with-eval-after-load "files"
    (add-function :override
                  (symbol-function 'file-truename)
                  #'override--file-truename))
  (with-eval-after-load "erc"
    (add-function :override
                  (symbol-function 'erc-identd-start)
                  #'override--erc-identd-start))
  (with-eval-after-load "polymode-core"
    (add-function :override
                  (symbol-function 'pm--run-other-hooks)
                  #'override--pm--run-other-hooks))

  (disable-file-truename-ad--set-advices "compile"
                                         '(compilation-find-file
                                           compilation-find-file))

  (disable-file-truename-ad--set-advices "projectile"
                                         '(projectile-cache-current-file
                                           delete-file-projectile-remove-from-cache
                                           projectile-project-root
                                           projectile-project-buffer-p
                                           projectile-select-files
                                           projectile-compilation-dir))

  (disable-file-truename-ad--set-advices "lsp-mode"
                                         '(
                                           lsp--all-watchable-directories
                                           lsp-watch-root-folder
                                           lsp--server-register-capability
                                           lsp--folder-watch-callback
                                           )))

;;;###autoload
(defun lotus-wrapper-uninsinuate ()
  (interactive)
  (remove-function (symbol-function 'file-truename)
                   #'override--file-truename)
  (remove-function (symbol-function 'erc-identd-start)
                   #'override--erc-identd-start)
  (remove-function (symbol-function 'pm--run-other-hooks)
                   #'override--pm--run-other-hooks)
  (disable-file-truename-ad--unset-advices "compile"
                                           '(compilation-find-file
                                             compilation-find-file))

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
                                             )))


;; from lsp-mode.el
;; lsp--all-watchable-directories
;; lsp-watch-root-folder
;; lsp--server-register-capability
;; lsp--folder-watch-callback

;; (file-truename "~/.mailbox")

;;; lotus-wrapper.el ends here





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

