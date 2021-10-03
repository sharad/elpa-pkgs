;;; occ-commands.el --- occ commands                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <>
;; Keywords:

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

(provide 'occ-commands)


(require 'occ)
(require 'occ-macros)
(require 'occ-util-common)
(require 'occ-main)
(require 'occ-cl-utils)
(require 'occ-obj-utils)
(require 'occ-resolve-clock)
(require 'occ-mode)


;; example of clos in cl-struct-js2-export-binding-node is a variable defined in ‘js2-mode.el’.

;;;###autoload
(defun occ-helm-match-select (ctx)
  (interactive
   (list (occ-obj-make-ctx-at-point)))
  (occ-obj-match-select ctx))

(defun occ-helm-list-select (ctx)
  (interactive
   (list (occ-obj-make-ctx-at-point)))
  (occ-obj-list-select ctx))

(defun occ-helm-list-debug-select (ctx)
  (interactive
   (list (occ-obj-make-ctx-at-point)))
  (occ-obj-list-debug-select ctx))

(defun occ-helm-list-launch (ctx)
  (interactive
   (list (occ-obj-make-ctx-at-point)))
  (occ-obj-list-launch ctx))

;;;###autoload
(defun occ-property-edit ()
  (interactive)
  (let ((ctx (occ-obj-make-ctx-at-point)))
    (occ-do-properties-window-editor ctx
                                     :ap-normal '(t actions edit)
                                     :ap-transf '(t actions edit))))


;;;###autoload
(defun occ-curr-procreate-child ()
  (interactive)
  (let ((ctxual-tsk (occ-current-ctxual-tsk)))
    (if ctxual-tsk
        (occ-do-procreate-child ctxual-tsk)
      (occ-message "No current task clocking-in"))))

;;;###autoload
(defun occ-curr-procreate-child-clock-in ()
  (interactive)
  (let ((ctxual-tsk (occ-current-ctxual-tsk)))
    (if ctxual-tsk
        (occ-do-procreate-child-clock-in ctxual-tsk)
      (occ-message "No current task clocking-in"))))

(defun occ-curr-tsk-continue-for (mins)
  (occ-error "Implement it."))


;;;###autoload
(defun occ-start-day ()
  (interactive)
  ;; also detect if day is started.
  (occ-error "Implement it."))

(defun occ-show-up (mins)
  (interactive)
  ;; https://www.merriam-webster.com/thesaurus/pack%20(up%20or%20off)
  (occ-error "Implement it."))

(defun occ-stop-day ()
  (interactive)
  (occ-error "Implement it."))

(defun occ-pack-up (mins)
  (interactive)
  ;; https://www.merriam-webster.com/thesaurus/pack%20(up%20or%20off)
  (occ-error "Implement it."))


;; action
(cl-defmethod occ-do-log-note ()
  (occ-error "Implement it."))

(cl-defmethod occ-do-curr-tsk-log-note ()
  (occ-error "Implement it."))


(defun occ-do-clock-in-force ()
  (occ-error "Implement it, open context ctx if not present, then occ-do-clock-in-if-associable else show error."))

(defun occ-interrupt-clock-in (mins)
  (occ-error "Implement it."))

(defun occ-clock-out (&optional switch-to-state
                                fail-quietly
                                at-time)
  (interactive)
  (org-clock-out switch-to-state
                 fail-quietly
                 at-time))

(defun occ-continue-prev ()
  (occ-error "Implement it."))

(defun occ-obj-make-anonymous ())

;; TODO: direct prop edit/add/replace/remove etc from helm menu


;; implement console.

;; TODO: direct prop edit/add/replace/remove etc from helm menu


(defvar occ-keep-quiet-timer nil)

(defun occ-keep-quiet ()
  (interactive)
  (occ-keep-quiet-for 7))

(defun occ-keep-quiet-for (mins)
  (interactive "Nmins: ")
  (when occ-keep-quiet-timer
    (cancel-timer occ-keep-quiet-timer)
    (setq occ-keep-quiet-timer nil))
  (setq occ-keep-quiet-timer
        (run-with-timer (* mins 60) nil
                        #'(lambda ()
                            (setq occ-keep-quiet-timer nil)
                            (occ-config-disable-quiet)
                            (occ-message "OCC noise ahead %s." (occ-config-value-quiet)))))
  (occ-config-enable-quiet)
  (occ-message "OCC Keeping quiet for %d mins" mins))


;;;###autoload
(defun occ-register-resolve-clock ()
  (interactive)
  (occ-rl-register-resolve-clock))

;;;###autoload
(defun occ-unregister-resolve-clock ()
  (interactive)
  (occ-rl-unregister-resolve-clock))

;;;###autoload
(defun occ-switch-default-key (key)
  (interactive (list (completing-read "key for spec: " (occ-collector-keys))))
  (occ-collector-default-key key))

;;;###autoload
(defun occ-reset-spec (key)
  (interactive  (list (completing-read "key for spec: " (occ-collector-keys))))
  (occ-reset-collection-object key)
  (occ-collector-remove key))

;;;###autoload
(defun occ-obj-make-spec (key desc)
  (interactive (list (completing-read "key for spec: " (occ-collector-keys))
                     (read-from-minibuffer "Desc: ")))
  (if (occ-collector-spec key)
      (occ-message "spec: %s already present, first reset it with occ-reset-spec"
                   (occ-collector-spec key))
    (let ((spec (completing-read "Spec: " (occ-specs))))
      (when spec
        (occ-collector-get-create key desc (intern spec) nil)
        ;; (push (intern spec)
        ;;       (occ-collector-spec key))
        (occ-reset-collection-object key)))))

;;;###autoload
(defun occ-add-to-spec (key file)
  (interactive (list (completing-read "key for spec: " (occ-collector-keys))
                     (read-file-name "org file:")))
  ;; TODO: Improve to create direct tree from here rather than resetting whole occ-global-tsk-collection
  (unless (occ-collector-spec key)
    (occ-obj-make-spec key))
  (if (occ-collector-spec key)
    (unless (memq file (rest (occ-collector-spec key)))
      (let ((spec       (first (occ-collector-spec key)))
            (spec-files (rest (occ-collector-spec key))))
        (setq spec-files
             (if current-prefix-arg
                 (nconc (list file) spec-files)
               (nconc spec-files (list file))))
        (setf (occ-collection-spec (occ-collector-get key)) (cons spec spec-files)))
      (prog1
          (occ-collector-spec key)
        (occ-reset-collection-object key)))))

;;;###autoload
(defun occ-add-org-file (key buffer)
  (interactive (list (completing-read "key for spec: " (occ-collector-keys))
                     (current-buffer)))
  (occ-do-add-org-buffer key buffer))

;;;###autoload
(defun occ-obj-build-spec (key)
  (interactive (list (completing-read "key for spec: " (occ-collector-keys))))
  (occ-obj-make-spec key)
  (when (first (occ-collector-spec key))
        (occ-add-to-spec key (read-file-name "Spec file: ")))
  (prog1
      (occ-collector-spec key)
    (occ-reset-collection-object key)))


;; testing verification
(defun occ-files-with-null-regex ()
  (interactive)
  (let ((files (remove-if #'(lambda (f)
                              (with-current-buffer (find-file-noselect f)
                                org-complex-heading-regexp))
                          (occ-obj-files))))
    (occ-message "files with null regex %s" files)))

;; testing verification
(defun occ-files-not-in-org-mode ()
  (interactive)
  (let ((files (remove-if #'(lambda (f)
                              (with-current-buffer (find-file-noselect f)
                                (eq major-mode 'org-mode)))
                          (occ-obj-files))))
    (occ-message "files not in org-mode %s" files)))


;;;###autoload
(defun occ-merge-unamed-task ()
  (interactive)
  (occ-error "Implement it."))


;;;###autoload
(defun occ-insinuate (&optional key)
  (interactive)
  (occ-message "occ-insinuate: begin")
  (prog1
      (occ-initialize key)
    (occ-message "occ-insinuate: finish")))

;;;###autoload
(defun occ-uninsinuate ()
  (interactive)
  (occ-message "occ-uninsinuate: begin")
  (prog1
      (occ-uninitialize)
    (occ-message "occ-uninsinuate: finish")))


;;;###autoload
(defun occ-run-timer ()
  (interactive)
  (occ-run-curr-ctx-timer))

(defvar occ-reload t)
;;;###autoload
(defun occ-reload (&optional uncompiled)
  (interactive "P")
  (when occ-reload
    (setq occ-reload nil)
    (occ-reload-lib uncompiled)
    (setq occ-reload t)))

(defun occ-version (&optional here full message)
  "Show the Occ version.
Interactively, or when MESSAGE is non-nil, show it in echo area.
With prefix argument, or when HERE is non-nil, insert it at point.
In non-interactive uses, a reduced version string is output unless
FULL is given."
  (interactive (list current-prefix-arg
                     t
                     (not current-prefix-arg)))
  (occ-message (occ-get-version here
                                full
                                message)))


(defun occ-run ()
  (interactive)
  (helm :prompt "test"
        :sources (list (helm-build-sync-source "Actions"
                           :candidates (list (cons "occ clock-in current context (force)"
                                                   #'(lambda () (occ-do-clock-in-curr-ctx t)))
                                             (cons "occ clock-in current context"
                                                   #'(lambda () (occ-do-clock-in-curr-ctx nil))))
                           :action     (list (cons "run"
                                                   #'(lambda (candidate-fun)
                                                       (funcall candidate-fun)))))
                       (occ-helm-build-extra-actions-ctx-buffer-source))))

;;; occ-commands.el ends here
