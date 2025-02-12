;;; lotus-projectile-advices.el --- projectile advices  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  sharad

;; Author: sharad <spratap@merunetworks.com>
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

(provide 'lotus-projectile-advices)



(defun override--projectile-project-root (&optional dir)
  "Retrieves the root directory of a project if available.
If DIR is not supplied its set to the current directory by default."
  (let ((dir (or dir default-directory)))
    ;; Back out of any archives, the project will live on the outside and
    ;; searching them is slow.
    (when (and (fboundp 'tramp-archive-file-name-archive)
               (tramp-archive-file-name-p dir))
      (setq dir (file-name-directory (tramp-archive-file-name-archive dir))))
    ;; the cached value will be 'none in the case of no project root (this is to
    ;; ensure it is not reevaluated each time when not inside a project) so use
    ;; cl-subst to replace this 'none value with nil so a nil value is used
    ;; instead
    (cl-subst nil 'none
              (or
               ;; if we've already failed to find a project dir for this
               ;; dir, and cached that failure, don't recompute
               (let* ((cache-key (format "projectilerootless-%s" dir))
                      (cache-value (gethash cache-key projectile-project-root-cache)))
                 cache-value)
               ;; if the file isn't local, and we're not connected, don't try to
               ;; find a root now now, but don't cache failure, as we might
               ;; re-connect.  The `is-local' and `is-connected' variables are
               ;; used to fix the behavior where Emacs hangs because of
               ;; Projectile when you open a file over TRAMP. It basically
               ;; prevents Projectile from trying to find information about
               ;; files for which it's not possible to get that information
               ;; right now.
               (let ((is-local (not (file-remote-p dir)))      ;; `true' if the file is local
                     (is-connected (file-remote-p dir nil t))) ;; `true' if the file is remote AND we are connected to the remote
                 (unless (or is-local is-connected)
                   'none))
               ;; if the file is local or we're connected to it via TRAMP, run
               ;; through the project root functions until we find a project dir
               (cl-some
                (lambda (func)
                  (let* ((cache-key (format "%s-%s" func dir))
                         (cache-value (gethash cache-key projectile-project-root-cache)))
                    (if (and cache-value (file-exists-p cache-value))
                        cache-value
                      (let ((value (funcall func dir)))
                        (puthash cache-key value projectile-project-root-cache)
                        value))))
                projectile-project-root-functions)
               ;; if we get here, we have failed to find a root by all
               ;; conventional means, and we assume the failure isn't transient
               ;; / network related, so cache the failure
               (let ((cache-key (format "projectilerootless-%s" dir)))
                 (puthash cache-key 'none projectile-project-root-cache)
                 'none)))))



;;; lotus-projectile-advices.el ends here
