;;; occ-obj-simple.el --- occ simple methods         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <>
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

(provide 'occ-obj-simple)


(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))
(require 'org-capture+-macros)
(eval-when-compile
  (require 'org-capture+-macros))
(require 'org-capture+)


(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj-accessor)
(require 'occ-obj-utils)
(require 'occ-util-common)
(require 'occ-print)
(require 'occ-predicate)
(require 'occ-rank)
(require 'occ-prop)
(require 'occ-property-rank-methods)
(require 'occ-list-filter)
(require 'occ-select)
(require 'occ-helm)
(require 'occ-clock)
(require 'occ-util-method)


(defvar occ-idle-timeout 7)


;; (cl-defmethod occ-find ((collection occ-collection) (mrk marker)))

(cl-defmethod occ-find ((collection list)
                        (mrk marker)))

(cl-defgeneric occ-goto (obj)
  "occ-goto")

(cl-defmethod occ-goto ((obj marker))
  (switch-to-buffer (marker-buffer obj))
  ;; TODO find about "org-overview"
  ;; https://stackoverflow.com/questions/25161792/emacs-org-mode-how-can-i-fold-everything-but-the-current-headline
  ;; https://emacs.stackexchange.com/questions/26827/test-whether-org-mode-heading-or-list-is-folded
  ;; https://github.com/facetframer/orgnav
  ;; https://stackoverflow.com/questions/6198339/show-org-mode-outline-up-to-a-certain-heading-level
  ;; (outline-show-all)
  (org-content 10)
  (goto-char obj))

(cl-defmethod occ-goto ((obj occ-obj-tsk))
  (let ((mrk (occ-obj-marker obj)))
    (if (and (markerp mrk)
             (marker-buffer mrk))
        (occ-goto mrk)
      (error "marker %s invalid." mrk))))


(cl-defgeneric occ-set-to (obj)
  "occ-set-to")

(cl-defmethod occ-set-to ((obj marker))
  (set-buffer (marker-buffer obj))
  ;; TODO find about "org-overview"
  ;; https://stackoverflow.com/questions/25161792/emacs-org-mode-how-can-i-fold-everything-but-the-current-headline
  ;; https://emacs.stackexchange.com/questions/26827/test-whether-org-mode-heading-or-list-is-folded
  ;; https://github.com/facetframer/orgnav
  ;; https://stackoverflow.com/questions/6198339/show-org-mode-outline-up-to-a-certain-heading-level
  ;; (outline-show-all)
  ;; (org-content 10)
  (goto-char obj))

(cl-defmethod occ-set-to ((obj occ-obj-tsk))
  (let ((mrk (occ-obj-marker obj)))
    (if (and (markerp mrk)
             (marker-buffer mrk))
        (occ-set-to mrk)
      (error "marker %s invalid." mrk))))


(cl-defmethod occ-induct-child ((obj   occ-tree-tsk)
                                (child occ-tree-tsk))
  (occ-set-property child 'subtree-level
                    (occ-get-property obj 'subtree-level))
  (occ-insert-node-after-element child obj
                                 (occ-tree-collection-list (occ-collection-object)))
  (setf (occ-tree-tsk-subtree obj) (nconc (occ-tree-tsk-subtree obj)
                                          (list  child))))

(cl-defmethod occ-induct-child ((obj   occ-list-tsk)
                                (child occ-list-tsk))
  (occ-set-property child 'subtree-level
                    (occ-get-property obj 'subtree-level))
  (occ-insert-node-after-element child obj
                                 (occ-tree-collection-list (occ-collection-object))))


(cl-defgeneric occ-capture (obj
                            &key
                            template
                            clock-in)
  "occ-capture")

(cl-defmethod occ-capture ((obj marker)
                           &key
                           template
                           clock-in)
  (org-capture-run
   'entry
   `(marker ,obj)
   'occ-capture+-helm-select-template
   :empty-lines 1))

(cl-defmethod occ-capture ((obj occ-tsk)
                           &key
                           template
                           clock-in)
  (let ((mrk (occ-tsk-marker obj)))
    (occ-capture mrk
                 :clock-in clock-in
                 :template template)))

(cl-defmethod occ-capture ((obj occ-obj-ctx-tsk)
                           &key
                           template
                           clock-in)
  (let ((mrk      (occ-obj-marker obj))
        (tsk      (occ-obj-tsk    obj))
        (ctx      (occ-obj-ctx    obj))
        (template (or template
                      (occ-capture+-helm-select-template))))
    (when template
      (with-org-capture-run marker 'entry `(marker ,mrk) template '(:empty-lines 1)
        (let* ((tmp-tsk  (occ-make-tsk marker))
               (tmp-ctsk (occ-build-ctsk-with tmp-tsk ctx)))
          (occ-props-edit tmp-ctsk)
          t)
        (let* ((child-tsk        (occ-make-tsk marker))
               (child-ctxual-tsk (occ-build-ctxual-tsk-with child-tsk ctx)))
          (when child-tsk
            (occ-induct-child tsk child-tsk)
            (when clock-in
              (occ-try-clock-in child-ctxual-tsk))))))))

(cl-defmethod occ-capture ((obj null)
                           &key
                           template
                           clock-in)
  (let ((ctx-tsk (occ-list-select (occ-make-ctx-at-point)
                                  :obtrusive t)))
    (occ-capture ctx-tsk
                 :template template
                 :clock-in clock-in)))


(cl-defgeneric occ-procreate-child (obj)
  "occ-child")

(cl-defmethod occ-procreate-child ((obj marker)
                                   &key
                                   template
                                   clock-in)
  (if (not (occ-unnamed-p obj))
      (occ-capture obj
                   :clock-in clock-in ;; helm-current-prefix-arg
                   :template template)
    (let ((title (occ-title obj 'captilize)))
     (error "%s is unnamed %s so can not create child "
           (occ-format obj 'captilize)
           title
           title))))

(cl-defmethod occ-procreate-child ((obj occ-obj-tsk)
                                   &key
                                   template
                                   clock-in)
  (if (not (occ-unnamed-p obj))
      (occ-capture obj
                   :clock-in clock-in ;; helm-current-prefix-arg
                   :template template)
    (let ((title (occ-title obj 'captilize)))
      (error "%s is unnamed %s so can not create child "
             (occ-format obj 'captilize)
             title
             title))))


(defun sacha/org-capture-prefill-template (template &rest values)
  "Pre-fill TEMPLATE with VALUES."
  (let ((template (or template)))
   (with-temp-buffer
     (insert template)
     (goto-char (point-min))
     (while (re-search-forward
             (concat "%\\("
                     "\\[\\(.+\\)\\]\\|"
                     "<\\([^>\n]+\\)>\\|"
                     "\\([tTuUaliAcxkKInfF]\\)\\|"
                     "\\(:[-a-zA-Z]+\\)\\|"
                     "\\^\\({\\([^}]*\\)}\\)"
                     "?\\([gGtTuUCLp]\\)?\\|"
                     "%\\\\\\([1-9][0-9]*\\)"
                     "\\)") nil t)
       (if (car values)
           (replace-match (car values) nil t))
       (setq values (cdr values)))
     (buffer-string))))

(defun sacha/helm-org-create-task (candidate)
  (let ((entry (org-capture-select-template "T")))
    (org-capture-set-plist entry)
    (org-capture-get-template)
    (org-capture-set-target-location)
    (condition-case error
        (progn
          (org-capture-put
           :template
           (org-capture-fill-template
            (sacha/org-capture-prefill-template (org-capture-get :template)
                                                candidate)))
          (org-capture-place-template
           (equal (car (org-capture-get :target)) 'function)))
      ((error quit)
       (if (get-buffer "*Capture*") (kill-buffer "*Capture*"))
       (error "Capture abort: %s" error)))) t)


(cl-defmethod occ-tsk-txt ((obj occ-obj-ctx)
                           (heading string))
  "Build a task name description from OBJ occ-ctx"
  (concat "* " heading "\n"))


(cl-defmethod occ-fast-procreate-child ((heading string)
                                        &key
                                        template
                                        clock-in)
  (let ((ctx (occ-make-ctx-at-point)))
    (occ-capture nil
                 :clock-in clock-in ;; helm-current-prefix-arg
                 :template (occ-tsk-txt ctx heading))))

(cl-defmethod occ-fast-procreate-anonymous-child ((heading string)
                                                  &key
                                                  template
                                                  clock-in)
  (let ((ctx (occ-make-ctx-at-point)))
    (occ-capture nil
                 :clock-in clock-in ;; helm-current-prefix-arg
                 :template (occ-tsk-txt ctx heading))))


(cl-defgeneric occ-procreate-child-clock-in (obj)
  "occ-child-clock-in")

(cl-defmethod occ-procreate-child-clock-in ((obj marker))
  (occ-capture obj
               :clock-in t))

(cl-defmethod occ-procreate-child-clock-in ((obj occ-obj-tsk))
  (occ-capture obj
               :clock-in t))


(defun occ-confirm (fn new)
  (occ-y-or-n-timeout)
  (error "Implement it."))

;;; occ-obj-simple.el ends here
