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
(require 'occ-debug-method)
(require 'occ-obj-method)
(require 'occ-helm-method)


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


(defun occ-capture+-helm-select-template ()
  (let ((selector (helm-template-gen-selector #'org-capture+-tree-predicate
                                              '(t occ tsk clockable todo)
                                              0)))
    (funcall selector)))

(occ-testing
 (occ-generate-plist-functions occ-helm action)
 ;; Will generate
 ;; * variable
 ;; occ-helm-action-plist
 ;; * functions
 ;; occ-helm-actions-clear
 ;; occ-helm-action-add
 ;; occ-helm-action-set
 ;; occ-helm-action-get
 ;; occ-helm-actions-get

 (cl-defgeneric occ-get-helm-actions-plist (obj
                                            name-actkey-list)
   "Return (list (NAME ACTION) ...) where for (CAR
NAME-ACTKEY-LIST) as NORMAL simple ACTION, while for (CAR
NAME-ACTKEY-LIST) as GENERATOR a generated ACTION based on OBJ
supplied.")

 (cl-defmethod occ-get-helm-actions-plist ((obj null)
                                           name-actkey-list)
   "Return (list (NAME ACTION) ...) where for (CAR
NAME-ACTKEY-LIST) as NORMAL simple ACTION, while for (CAR
NAME-ACTKEY-LIST) as GENERATOR a generated ACTION based on OBJ
supplied."
   (cond
    ((eql (car name-actkey-list) 'normal)
     (apply #'occ-helm-actions-get (cdr name-actkey-list)))
    ((eql (car name-actkey-list) 'generator)
     (when obj
       ;; Here a action or generator action (cdr generator) will generate multiple action/callable
       (apply #'append
              (mapcar #'(lambda (generator)
                          (funcall (cdr generator) obj :param-only nil))
                      (apply #'occ-helm-actions-get (cdr name-actkey-list))))))))

 (cl-defmethod occ-get-helm-actions-plist ((obj occ-obj)
                                           name-actkey-list)
   "Return (list (NAME ACTION) ...) where for (CAR
NAME-ACTKEY-LIST) as NORMAL simple ACTION, while for (CAR
NAME-ACTKEY-LIST) as GENERATOR a generated ACTION based on OBJ
supplied."
   (occ-get-helm-actions-plist nil name-actkey-list))

 (cl-defmethod occ-get-helm-actions-plist ((obj occ-obj-tsk)
                                           name-actkey-list)
   "Return (list (NAME ACTION) ...) where for (CAR
NAME-ACTKEY-LIST) as NORMAL simple ACTION, while for (CAR
NAME-ACTKEY-LIST) as GENERATOR a generated ACTION based on OBJ
supplied."
   (cond
    ((eql (car name-actkey-list) 'normal)
     (apply #'occ-helm-actions-get (cdr name-actkey-list)))
    ((eql (car name-actkey-list) 'generator)
     (when obj
       ;; Here a action or generator action (cdr generator) will generate multiple action/callable
       (apply #'append
              (mapcar #'(lambda (generator)
                          (funcall (cdr generator) obj :param-only nil))
                      (apply #'occ-helm-actions-get (cdr name-actkey-list))))))))
 ;; TODO: use OCC-MAKE-CALLABLE-NORMAL interface
 (progn
   (occ-helm-action-clear)
   (occ-helm-action-add :ignore                   "Ignore"                   #'ignore)
   (occ-helm-action-add :identity                 "Select"                   #'identity)
   (occ-helm-action-add :clock-in                 "Clock-in"                 #'occ-clock-in)
   (occ-helm-action-add :try-fast-clock-in        "Try Fast Clock-in"        #'occ-try-fast-clock-in)
   (occ-helm-action-add :try-clock-in             "Try Clock-in"             #'occ-try-clock-in)
   (occ-helm-action-add :procreate-child          "Procreate Child"          #'occ-procreate-child)
   (occ-helm-action-add :procreate-child-clock-in "Procreate Child Clock-in" #'occ-procreate-child-clock-in)
   (occ-helm-action-add :goto                     "Goto"                     #'occ-goto)
   (occ-helm-action-add :set-to                   "Set To"                   #'occ-set-to)
   (occ-helm-action-add :proprty-window-edit      "Proprtes Window Edit"     #'occ-props-window-edit) ;TODO: implement it.
   (occ-helm-action-add :proprty-edit-combined    "Proprtes Edit Combined"   #'occ-props-edit-combined) ;TODO: implement it.
   (occ-helm-action-add :call-with-obj            "Call with object"         #'occ-call-with-obj)
   (occ-helm-action-add :set-debug-obj            "Set debug obj"            #'occ-set-debug-obj)
   (occ-helm-action-add :rank                     "Get Rank"                 #'occ-print-rank)
   (occ-helm-action-add :tsk                      "Get Task"                 #'occ-print-tsk))

 (progn
   (occ-helm-action-add :fast-edits-gen     "Fast Edits" #'occ-gen-helm-fast-edits)
   (occ-helm-action-add :edits-gen          "Edit"       #'occ-gen-helm-edits)
   (occ-helm-action-add :misc-gen           "Misc"       #'occ-gen-helm-misc)
   (occ-helm-action-add :fast-checkouts-gen "Checkouts"  #'occ-gen-helm-checkouts))

 (occ-testing

  (occ-get-alist-from-tree '(t actions general))

  (tree-collect-items occ-helm-actions-tree nil '(t actions general) 0)

  (occ-get-helm-actions-plist nil
                              (car (occ-get-alist-from-tree '(t actions general))))

  (apply #'append
         (mapcar #'(lambda (name-action-key)
                     (occ-get-helm-actions-plist nil name-action-key))
                 (occ-get-alist-from-tree '(t actions general)))))
 ())  


(defvar occ-helm-callables)
(defun occ-helm-callable-add (callable)
  (push callable occ-helm-callables))
(defun occ-helm-callables-get (keylist)
  (remove-if-not #'(lambda (callable) (memq (occ-callable-keyword callable) keylist))
                 occ-helm-callables))

;; replacement of (occ-get-helm-actions-plist)
(cl-defmethod occ-get-callables ((obj occ-obj-tsk) keylist)
  (append (mapcar #'(lambda (callable)
                      (occ-callable-methods callable obj))
                  (occ-helm-callables-get keylist))))


(progn
  (setq occ-helm-callables nil)
  (occ-helm-callable-add (occ-make-callable-normal  :ignore                   "Ignore"                   #'ignore))
  (occ-helm-callable-add (occ-make-callable-normal  :identity                 "Select"                   #'identity))
  (occ-helm-callable-add (occ-make-callable-normal  :clock-in                 "Clock-in"                 #'occ-clock-in))
  (occ-helm-callable-add (occ-make-callable-normal  :try-fast-clock-in        "Try Fast Clock-in"        #'occ-try-fast-clock-in))
  (occ-helm-callable-add (occ-make-callable-normal  :try-clock-in             "Try Clock-in"             #'occ-try-clock-in))
  (occ-helm-callable-add (occ-make-callable-normal  :procreate-child          "Procreate Child"          #'occ-procreate-child))
  (occ-helm-callable-add (occ-make-callable-normal  :procreate-child-clock-in "Procreate Child Clock-in" #'occ-procreate-child-clock-in))
  (occ-helm-callable-add (occ-make-callable-normal  :goto                     "Goto"                     #'occ-goto))
  (occ-helm-callable-add (occ-make-callable-normal  :set-to                   "Set To"                   #'occ-set-to))
  (occ-helm-callable-add (occ-make-callable-normal  :proprty-window-edit      "Proprtes Window Edit"     #'occ-props-window-edit)) ;TODO: implement it.
  (occ-helm-callable-add (occ-make-callable-normal  :proprty-edit-combined    "Proprtes Edit Combined"   #'occ-props-edit-combined)) ;TODO: implement it.
  (occ-helm-callable-add (occ-make-callable-normal  :call-with-obj            "Call with object"         #'occ-call-with-obj))
  (occ-helm-callable-add (occ-make-callable-normal  :set-debug-obj            "Set debug obj"            #'occ-set-debug-obj))
  (occ-helm-callable-add (occ-make-callable-normal  :rank                     "Get Rank"                 #'occ-print-rank))
  (occ-helm-callable-add (occ-make-callable-normal  :tsk                      "Get Task"                 #'occ-print-tsk)))


(defvar occ-helm-actions-tree '(t))

(setq occ-helm-actions-tree '(t))

(defun occ-add-helm-actions (keys class &rest actions)
  (apply #'tree-add-class-item
         occ-helm-actions-tree
         keys
         class
         actions))

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
                      :proprty-edit-combined)


(occ-helm-callable-add (occ-make-callable-generator  :fast-edits-gen     "Fast Edits" #'occ-gen-helm-fast-edits))
(occ-helm-callable-add (occ-make-callable-generator  :edits-gen          "Edit"       #'occ-gen-helm-edits))
(occ-helm-callable-add (occ-make-callable-generator  :misc-gen           "Misc"       #'occ-gen-helm-misc))
(occ-helm-callable-add (occ-make-callable-generator  :fast-checkouts-gen "Checkouts"  #'occ-gen-helm-checkouts))

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
                      :fast-checkouts-gen)


;; (defun occ-get-alist-from-tree (keys)
;;   (collect-alist (tree-collect-items occ-helm-actions-tree nil keys 0)))

(defun occ-get-alist-from-tree (keys-tree-branch)
  (tree-collect-items occ-helm-actions-tree
                      nil
                      keys-tree-branch
                      0))


(cl-defgeneric occ-get-helm-actions (obj keys-tree-branch)
  "occ-get-helm-actions")

(cl-defmethod occ-get-helm-actions ((obj null) keys-tree-branch)
  ;; (occ-message "occ-get-helm-actions: called with obj = %s, keys-tree-branch = %s" obj keys-tree-branch)
  (append (occ-get-callables obj
                             (occ-get-alist-from-tree keys-tree-branch))))
         
(cl-defmethod occ-get-helm-actions ((obj occ-obj) keys-tree-branch)
  ;; (occ-message "occ-get-helm-actions: called with obj = %s, keys-tree-branch = %s" obj keys-tree-branch)
  (append (occ-get-callables obj
                             (occ-get-alist-from-tree keys-tree-branch))))

(cl-defmethod occ-get-helm-actions-genertator ((obj null) keys-tree-branch)
  #'(lambda (action candidate)
      (occ-get-callables candidate keys-tree-branch)))

(cl-defmethod occ-get-helm-actions-genertator ((obj occ-obj) keys-tree-branch)
  #'(lambda (action candidate)
      (occ-get-callables candidate keys-tree-branch)))


(occ-testing
 (cl-defgeneric occ-get-helm-actions (obj keys)
   "occ-get-helm-actions")

 (cl-defmethod occ-get-helm-actions ((obj null) keys)
   ;; (occ-message "occ-get-helm-actions: called with obj = %s, keys = %s" obj keys)
   (apply #'append
          (mapcar #'(lambda (name-action-key)
                      (occ-get-helm-actions-plist obj name-action-key))
                  (occ-get-alist-from-tree keys))))

 (cl-defmethod occ-get-helm-actions ((obj occ-obj) keys)
   ;; (occ-message "occ-get-helm-actions: called with obj = %s, keys = %s" obj keys)
   (apply #'append
          (mapcar #'(lambda (name-action-key)
                      (occ-get-helm-actions-plist obj name-action-key))
                  (occ-get-alist-from-tree keys))))

 (occ-testing
  (tree-collect-items occ-helm-actions-tree nil '(t actions general edit) 0)
  (tree-collect-items occ-helm-actions-tree (occ-make-ctx-at-point) '(t actions general edit) 0)
  (occ-get-helm-actions (occ-make-ctx-at-point) '(t actions general edit)))

 (cl-defmethod occ-get-helm-actions-genertator ((obj null) keys)
   #'(lambda (action candidate)
       (occ-get-helm-actions candidate keys)))

 (cl-defmethod occ-get-helm-actions-genertator ((obj occ-obj) keys)
   #'(lambda (action candidate)
       (occ-get-helm-actions candidate keys)))
 ())

(occ-testing
  (collect-alist (tree-collect-items occ-helm-actions-tree nil '(t actions select) 0))
  (occ-get-helm-actions-plist nil '(normal :identity))
  (occ-helm-actions-get :edits-gen)
  (occ-helm-actions-get :identity)
  (occ-helm-actions-get :identity :clock-in)
  (occ-helm-actions-get :clock-in)
  (occ-get-helm-actions nil '(t actions select)))

;;; occ-helm.el ends here
