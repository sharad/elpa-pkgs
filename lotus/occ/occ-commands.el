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
(occ-defcommand occ-helm-match-select (ctx)
  (interactive
   (list (occ-make-ctx-at-point)))
  (occ-match-select ctx))

(occ-defcommand occ-helm-list-select (ctx)
  (interactive
   (list (occ-make-ctx-at-point)))
  (occ-list-select ctx))

(occ-defcommand occ-helm-list-debug-select (ctx)
  (interactive
   (list (occ-make-ctx-at-point)))
  (occ-list-debug-select ctx))

(occ-defcommand occ-helm-list-launch (ctx)
  (interactive
   (list (occ-make-ctx-at-point)))
  (occ-list-launch ctx))

;;;###autoload
(occ-defcommand occ-proprty-edit ()
  (interactive)
  (let ((ctx (occ-make-ctx-at-point)))
    (occ-props-window-edit ctx
                           :action             (occ-get-helm-actions-tree ctx '(t actions edit))
                           :action-transformer (occ-get-helm-actions-tree-genertator ctx '(t actions edit)))))


;;;###autoload
(occ-defcommand occ-curr-procreate-child ()
  (interactive)
  (let ((ctxual-tsk (occ-current-ctxual-tsk)))
    (if ctxual-tsk
        (occ-procreate-child ctxual-tsk)
      (occ-message "No current task clocking-in"))))

;;;###autoload
(occ-defcommand occ-curr-procreate-child-clock-in ()
  (interactive)
  (let ((ctxual-tsk (occ-current-ctxual-tsk)))
    (if ctxual-tsk
        (occ-procreate-child-clock-in ctxual-tsk)
      (occ-message "No current task clocking-in"))))

(occ-defcommand occ-curr-tsk-continue-for (mins)
  (error "Implement it."))


;;;###autoload
(occ-defcommand occ-start-day ()
  (interactive)
  ;; also detect if day is started.
  (error "Implement it."))

(occ-defcommand occ-show-up (mins)
  (interactive)
  ;; https://www.merriam-webster.com/thesaurus/pack%20(up%20or%20off)
  (error "Implement it."))

(occ-defcommand occ-stop-day ()
  (interactive)
  (error "Implement it."))

(occ-defcommand occ-pack-up (mins)
  (interactive)
  ;; https://www.merriam-webster.com/thesaurus/pack%20(up%20or%20off)
  (error "Implement it."))

;; action
(cl-defmethod occ-log-not ()
  (error "Implement it."))

(cl-defmethod occ-curr-tsk-log-not ()
  (error "Implement it."))


(occ-defcommand occ-clock-in-force ()
  (error "Implement it, open context ctx if not present, then occ-clock-in-if-associable else show error."))

(occ-defcommand occ-interrupt-clock-in (mins)
  (error "Implement it."))

(occ-defcommand occ-clock-out (&optional switch-to-state
                                fail-quietly
                                at-time)
  (interactive)
  (org-clock-out switch-to-state
                 fail-quietly
                 at-time))

(occ-defcommand occ-continue-prev ()
  (error "Implement it."))

(occ-defcommand occ-make-anonymous ())

;; TODO: direct prop edit/add/replace/remove etc from helm menu


;; implement console.

;; TODO: direct prop edit/add/replace/remove etc from helm menu


(defvar occ-keep-quiet-timer nil)

(occ-defcommand occ-keep-quiet ()
  (interactive)
  (occ-keep-quiet-for 7))

(occ-defcommand occ-keep-quiet-for (mins)
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
(occ-defcommand occ-register-resolve-clock ()
  (interactive)
  (occ-rl-register-resolve-clock))

;;;###autoload
(occ-defcommand occ-unregister-resolve-clock ()
  (interactive)
  (occ-rl-unregister-resolve-clock))


;;;###autoload
(occ-defcommand occ-reset-spec ()
  (interactive)
  (setq occ-global-tsk-collection-spec nil))

;;;###autoload
(occ-defcommand occ-make-spec ()
  (interactive)
  (if occ-global-tsk-collection-spec
      (occ-message "spec: %s already present, first reset it with occ-reset-spec"
                   occ-global-tsk-collection-spec)
    (let ((spec (completing-read "Spec: " (occ-specs))))
      (when spec
        (push (intern spec)
              occ-global-tsk-collection-spec)
        (occ-reset-collection-object)))))

;;;###autoload
(occ-defcommand occ-add-to-spec (file)
  (interactive "FSpec file: ")
  ;; TODO: Improve to create direct tree from here rather than resetting whole occ-global-tsk-collection
  (unless (memq file (cdr occ-global-tsk-collection-spec))
    (let ((spec       (car occ-global-tsk-collection-spec))
          (spec-files (cdr occ-global-tsk-collection-spec)))
      (setq spec-files
           (if current-prefix-arg
               (nconc (list file) spec-files)
             (nconc spec-files (list file))))
      (setq occ-global-tsk-collection-spec (cons spec spec-files)))
    (prog1
        occ-global-tsk-collection-spec
      (occ-reset-collection-object))))

;;;###autoload
(occ-defcommand occ-add-org-file (buffer)
  (interactive (list (current-buffer)))
  (occ-add-org-buffer buffer))

;;;###autoload
(occ-defcommand occ-build-spec ()
  (interactive)
  (occ-make-spec)
  (when (car occ-global-tsk-collection-spec)
        (occ-add-to-spec (read-file-name "Spec file: ")))
  (prog1
      occ-global-tsk-collection-spec
    (occ-reset-collection-object)))


;; testing verification
(occ-defcommand occ-files-with-null-regex ()
  (interactive)
  (let ((files
         (remove-if
          #'(lambda (f)
              (with-current-buffer (find-file-noselect f)
                org-complex-heading-regexp))
          (occ-files))))
    (occ-message "files with null regex %s" files)))

;; testing verification
(occ-defcommand occ-files-not-in-org-mode ()
  (interactive)
  (let ((files
         (remove-if
          #'(lambda (f)
              (with-current-buffer (find-file-noselect f)
                (eq major-mode 'org-mode)))
          (occ-files))))
    (occ-message "files not in org-mode %s" files)))


;;;###autoload
(occ-defcommand occ-merge-unamed-task ()
  (interactive)
  (error "Implement it."))


;;;###autoload
(occ-defcommand occ-reset-collection-object ()
  (interactive)
  (setq occ-global-tsk-collection nil)
  occ-global-tsk-collection)

;;;###autoload
(occ-defcommand occ-insinuate (&optional spec)
  (interactive)
  (occ-message "occ-insinuate: begin")
  (occ-initialize spec)
  (occ-message "occ-insinuate: finish"))

;;;###autoload
(occ-defcommand occ-uninsinuate ()
  (interactive)
  (occ-message "occ-uninsinuate: begin")
  (occ-uninitialize)
  (occ-message "occ-uninsinuate: finish"))


;;;###autoload
(occ-defcommand occ-run-timer ()
  (interactive)
  (occ-run-curr-ctx-timer))

(occ-defcommand occ-reload (&optional uncompiled)
  (interactive "P")
  (occ-reload-lib uncompiled))

(occ-defcommand occ-version (&optional here full message)
  "Show the Occ version.
Interactively, or when MESSAGE is non-nil, show it in echo area.
With prefix argument, or when HERE is non-nil, insert it at point.
In non-interactive uses, a reduced version string is output unless
FULL is given."
  (interactive
   (list
    current-prefix-arg
    t
    (not current-prefix-arg)))
  (occ-message (occ-get-version here full message)))

;;; occ-commands.el ends here
