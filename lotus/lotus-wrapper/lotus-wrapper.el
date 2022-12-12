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

;;; lotus-wrapper.el ends here





