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
(defun occ-proprty-edit ()
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
(cl-defmethod occ-do-log-not ()
  (occ-error "Implement it."))

(cl-defmethod occ-do-curr-tsk-log-not ()
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
(defun occ-reset-spec ()
  (interactive)
  (occ-reset-collection-object)
  (setq (occ-collections-map-spec key) nil))

;;;###autoload
(defun occ-obj-make-spec ()
  (interactive)
  (if (occ-collections-map-spec key)
      (occ-message "spec: %s already present, first reset it with occ-reset-spec"
                   (occ-collections-map-spec key))
    (let ((spec (completing-read "Spec: " (occ-specs))))
      (when spec
        (push (intern spec)
              (occ-collections-map-spec key))
        (occ-reset-collection-object)))))

;;;###autoload
(defun occ-add-to-spec (file)
  (interactive "FSpec file: ")
  ;; TODO: Improve to create direct tree from here rather than resetting whole (occ-collections-map-get key)
  (unless (occ-collections-map-spec key)
    (occ-obj-make-spec))
  (if (occ-collections-map-spec key)
    (unless (memq file (rest (occ-collections-map-spec key)))
      (let ((spec       (first (occ-collections-map-spec key)))
            (spec-files (rest (occ-collections-map-spec key))))
        (setq spec-files
             (if current-prefix-arg
                 (nconc (list file) spec-files)
               (nconc spec-files (list file))))
        (setq (occ-collections-map-spec key) (cons spec spec-files)))
      (prog1
          (occ-collections-map-spec key)
        (occ-reset-collection-object)))))

;;;###autoload
(defun occ-add-org-file (buffer)
  (interactive (list (current-buffer)))
  (occ-do-add-org-buffer buffer))

;;;###autoload
(defun occ-obj-build-spec ()
  (interactive)
  (occ-obj-make-spec)
  (when (first (occ-collections-map-spec key))
        (occ-add-to-spec (read-file-name "Spec file: ")))
  (prog1
      (occ-collections-map-spec key)
    (occ-reset-collection-object)))


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
(defun occ-reset-collection-object ()
  (interactive)
  (setq (occ-collections-map-get key) nil)
  (occ-collections-map-get key))

;;;###autoload
(defun occ-insinuate (&optional spec)
  (interactive)
  (occ-message "occ-insinuate: begin")
  (occ-initialize spec)
  (occ-message "occ-insinuate: finish"))

;;;###autoload
(defun occ-uninsinuate ()
  (interactive)
  (occ-message "occ-uninsinuate: begin")
  (occ-uninitialize)
  (occ-message "occ-uninsinuate: finish"))


;;;###autoload
(defun occ-run-timer ()
  (interactive)
  (occ-run-curr-ctx-timer))

;;;###autoload
(defun occ-reload (&optional uncompiled)
  (interactive "P")
  (occ-reload-lib uncompiled))

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
                       (occ-helm-build-ignore-ctx-buffer-source))))

;;; occ-commands.el ends here
