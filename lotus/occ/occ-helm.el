;;; occ-helm.el --- occ helm                         -*- lexical-binding: t; -*-

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

(provide 'occ-helm)


(require 'lotus-tree-manager)
(require 'org-capture+-helm)
(require 'org-capture+-helm-dynamic)
(require 'lotus-helm)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-macros)
(require 'occ-obj)
(require 'occ-obj-ctor)
(require 'occ-debug-method)
(require 'occ-obj-method)
(require 'occ-helm-method)


(defun occ-capture+-helm-select-template ()
  (let ((selector (helm-template-gen-selector #'org-capture+-tree-predicate
                                              '(t occ tsk clockable todo)
                                              0)))
    (funcall selector)))


(defvar occ-helm-callables)
(defun occ-helm-callable-add (callable)
  (push callable occ-helm-callables))
;; (defun occ-helm-callables-get (keylist)
;;   (remove-if-not #'(lambda (callable) (memq (occ-callable-keyword callable) keylist))
;;                  occ-helm-callables))
(defun occ-helm-callables-get (keylist)
  (apply #'append
         (mapcar #'(lambda (key)
                     (remove-if-not #'(lambda (callable) (eq (occ-callable-keyword callable) key))
                                    occ-helm-callables))
                 keylist)))
;; ;; replacement of (occ-get-helm-actions-plist)
;; (cl-defmethod occ-get-callables ((obj occ-obj-tsk) keylist)
;;   ;; TODO: do we require (apply #'append ...)
;;   (append (mapcar #'(lambda (callable)
;;                       (occ-callable-methods callable obj))
;;                   (occ-helm-callables-get keylist))))

;; (cl-defmethod occ-get-callables ((obj occ-obj) keylist)
;;   ;; TODO: do we require (apply #'append ...)
;;   (append (mapcar #'(lambda (callable)
;;                       (occ-callable-methods callable obj))
;;                   (occ-helm-callables-get keylist))))


;; replacement of (occ-get-helm-actions-plist)
(cl-defmethod occ-get-callables ((obj occ-obj-tsk)
                                 keylist)
  ;; TODO: do we require (apply #'append ...)
  (apply #'append
         (mapcar #'(lambda (callable)
                     (occ-obj-callables callable obj))
                 (occ-helm-callables-get keylist))))

(cl-defmethod occ-get-callables ((obj occ-obj)
                                 keylist)
  ;; TODO: do we require (apply #'append ...)
  (apply #'append
         (mapcar #'(lambda (callable)
                     (occ-obj-callables callable obj))
                 (occ-helm-callables-get keylist))))


(defvar occ-helm-actions-tree '(t))

(setq occ-helm-actions-tree '(t))

(defun occ-add-helm-actions (tree-keybranch class &rest actions)
  (apply #'tree-add-class-item
         occ-helm-actions-tree
         tree-keybranch
         class
         actions))


;; (defun occ-get-keywords-list-from-tree (keys)
;;   (collect-alist (tree-collect-items occ-helm-actions-tree nil keys 0)))

(defun occ-get-keywords-list-from-tree (tree-keybranch)
  (tree-collect-items occ-helm-actions-tree ;tree
                      nil                   ;predicate
                      tree-keybranch      ;arg
                      0))                   ;level


(cl-defgeneric occ-get-helm-actions (obj tree-keybranch)
  "occ-get-helm-actions")

(cl-defmethod occ-get-helm-actions ((obj null) tree-keybranch)
  ;; (occ-message "occ-get-helm-actions: called with obj = %s, tree-keybranch = %s" obj tree-keybranch)
  (apply #'append (occ-get-callables obj
                                     (occ-get-keywords-list-from-tree tree-keybranch))))

(cl-defmethod occ-get-helm-actions ((obj occ-obj) tree-keybranch)
  ;; (occ-message "occ-get-helm-actions: called with obj = %s, tree-keybranch = %s" obj tree-keybranch)
  (apply #'append (occ-get-callables obj
                                     (occ-get-keywords-list-from-tree tree-keybranch))))

(cl-defmethod occ-get-helm-actions-genertator ((obj null) tree-keybranch)
  #'(lambda (action candidate)
      (occ-get-helm-actions candidate tree-keybranch)))

(cl-defmethod occ-get-helm-actions-genertator ((obj occ-obj) tree-keybranch)
  #'(lambda (action candidate)
      (occ-get-helm-actions candidate tree-keybranch)))




;; TODO
;;;###autoload
(org-capture+-add-heading-template '(occ tsk clockable todo) "TODO"
                                   "* TODO %? %^g\n %i\n [%a]\n")
;;;###autoload
(org-capture+-add-heading-template '(occ tsk clockable todo) "TODO"
                                   "* MILESTONE %? %^g\n %i\n [%a]\n")
;;;###autoload
(org-capture+-add-heading-template '(occ tsk clockable meeting) "MEETING"
                                   "* MEETING %? %^g\n %i\n [%a]\n")

;; NOTE
;;;###autoload
(org-capture+-add-heading-template '(occ tsk unclockable note) "NOTE"
                                   "* <NOTE> %? %^g\n %i\n [%a]\n")
;; INFO
;;;###autoload
(org-capture+-add-heading-template '(occ tsk unclockable info) "INFO"
                                   "* <INFO> %? %^g\n %i\n [%a]\n")
;; EVENT
;;;###autoload
(org-capture+-add-heading-template '(occ tsk unclockable event) "EVENT"
                                   "* <EVENT> %? %^g\n %i\n [%a]\n")


(progn
  (progn
    (progn
      (setq occ-helm-callables nil)
      (occ-build-callable-normal :ignore                   "Ignore"                   #'ignore)
      (occ-build-callable-normal :identity                 "Select"                   #'identity)
      (occ-build-callable-normal :clock-in                 "Clock-in"                 #'occ-clock-in)
      (occ-build-callable-normal :try-fast-clock-in        "Try Fast Clock-in"        #'occ-try-fast-clock-in)
      (occ-build-callable-normal :try-clock-in             "Try Clock-in"             #'occ-try-clock-in)
      (occ-build-callable-normal :procreate-child          "Procreate Child"          #'occ-procreate-child)
      (occ-build-callable-normal :procreate-child-clock-in "Procreate Child Clock-in" #'occ-procreate-child-clock-in)
      (occ-build-callable-normal :goto                     "Goto"                     #'occ-goto)
      (occ-build-callable-normal :set-to                   "Set To"                   #'occ-set-to)
      (occ-build-callable-normal :proprty-window-edit      "Proprtes Window Edit"     #'occ-props-window-edit) ;TODO: implement it.
      (occ-build-callable-normal :proprty-edit-combined    "Proprtes Edit Combined"   #'occ-props-edit-combined) ;TODO: implement it.
      (occ-build-callable-normal :call-with-obj            "Call with object"         #'occ-call-with-obj)
      (occ-build-callable-normal :set-debug-obj            "Set debug obj"            #'occ-set-debug-obj)
      (occ-build-callable-normal :rank                     "Get Rank"                 #'occ-print-rank)
      (occ-build-callable-normal :tsk                      "Get Task"                 #'occ-print-tsk))
    (progn
      (occ-build-callable-generator :fast-edits-gen     "Fast Edits" #'occ-gen-helm-fast-edits)
      (occ-build-callable-generator :edits-gen          "Edit"       #'occ-gen-helm-edits)
      (occ-build-callable-generator :misc-gen           "Misc"       #'occ-gen-helm-misc)
      (occ-build-callable-generator :fast-checkouts-gen "Checkouts"  #'occ-gen-helm-checkouts)))

  (progn
    (progn
      (occ-add-helm-actions '(actions select)
                            "Select"
                            ;; 'normal
                            :identity)

      (occ-add-helm-actions '(actions general)
                            "Simple"
                            ;; 'normal
                            :procreate-child
                            :procreate-child-clock-in
                            :call-with-obj
                            :set-debug-obj
                            :try-clock-in
                            :goto
                            :rank
                            :tsk)

      (occ-add-helm-actions '(actions edit)
                            "Editing"
                            ;; 'normal
                            :proprty-window-edit
                            :proprty-edit-combined))
    (progn
      (occ-add-helm-actions '(actions general)
                            "General"
                            ;; 'generator
                            :misc-gen)

      (occ-add-helm-actions '(actions edit)
                            "Editing"
                            ;; 'generator
                            :fast-edits-gen
                            :edits-gen)

      (occ-add-helm-actions '(actions checkout)
                            "Checkout"
                            ;; 'generator
                            :fast-checkouts-gen))))















(occ-testing
 (tree-collect-items occ-helm-actions-tree nil '(t actions general edit) 0)
 (tree-collect-items occ-helm-actions-tree (occ-make-ctx-at-point) '(t actions general edit) 0)
 (occ-get-helm-actions (occ-make-ctx-at-point) '(t actions general edit)))

(occ-testing
  (collect-alist (tree-collect-items occ-helm-actions-tree nil '(t actions select) 0))
  (occ-helm-callables-get :edits-gen)
  (occ-helm-callables-get :identity)
  (occ-helm-callables-get :identity :clock-in)
  (occ-helm-callables-get :clock-in)
  (occ-get-helm-actions nil '(t actions select)))

(occ-testing
 (occ-get-keywords-list-from-tree '(t actions select))
 (occ-get-keywords-list-from-tree '(t actions general))
 (occ-get-keywords-list-from-tree '(t actions general edit))
 (occ-get-keywords-list-from-tree '(t actions select general edit))
 (occ-helm-callables-get (occ-get-keywords-list-from-tree '(t actions select general edit)))
 (occ-get-callables (occ-make-ctx-at-point)
                    (occ-get-keywords-list-from-tree '(t actions select general edit)))

 (car (occ-get-callables (occ-make-ctx-at-point)
                          (occ-get-keywords-list-from-tree '(t actions select general edit))))

 (tree-collect-items occ-helm-actions-tree nil '(t actions general) 0)

 (occ-get-helm-actions-plist nil
                             (car (occ-get-keywords-list-from-tree '(t actions general))))

 (apply #'append
        (mapcar #'(lambda (name-action-key)
                    (occ-get-helm-actions-plist nil name-action-key))
                (occ-get-keywords-list-from-tree '(t actions general)))))

;;; occ-helm.el ends here
