;;; occ-obj-ctor.el --- occ-api               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
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

;; https://www.gnu.org/software/emacs/manual/html_node/eieio/Quick-Start.html#Quick-Start
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Generic-Functions.html
;; The type specializer, (arg type), can specify one of the system types in the
;; following list. When a parent type is specified, an argument whose type is
;; any of its more specific child types, as well as grand-children,
;; grand-grand-children, etc. will also be compatible.
;;
;; integer (Parent type: number.)
;; number
;; null (Parent type: symbol.)
;; symbol
;; string (Parent type: array.)
;; array (Parent type: sequence.)
;; cons (Parent type: list.)
;; list (Parent type: sequence.)
;; marker
;; overlay
;; float (Parent type: number.)
;; window-configuration
;; process
;; window
;; subr
;; compiled-function
;; buffer
;; char-table (Parent type: array.)
;; bool-vector (Parent type: array.)
;; vector (Parent type: array.)
;; frame
;; hash-table
;; font-spec
;; font-entity
;; font-object

;;; Code:

(provide 'occ-obj-ctor)


(require 'seq)
(require 'list-utils)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-macros)
(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj)
;; (require 'occ-prop)
(require 'occ-rank)
(require 'occ-property-methods)


(defvar occ-global-tsk-collection-spec        nil)


(defvar occ-global-tsk-collection             nil)
(defvar occ-global-tsk-collection-change-hook nil
  "run when occ-global-tsk-collection-change-hook get changed.")


(defun occ-init-collector (type)
  (let ((collector (cond
                    ((eq type :tree) (make-occ-tree-collector))
                    ((eq type :list) (make-occ-list-collector)))))
    (defun occ-collector ()
      collector)
    (defun occ-collector-get (key)
      (rest (assoc key (occ-obj-collector-alist collector))))
    (defun occ-collector-set (key value)
      (setcdr (assoc key (occ-obj-collector-alist collector))
              value))
    (defun occ-collector-root (key)
      (let ((colection (occ-collector-get key)))
        (occ-collection-root collection)))
    (defun occ-collector-spec (key)
      (let ((colection (occ-collector-get key)))
        (occ-collection-spec collection)))))
(occ-init-collector :tree)
;; (occ-collector)


;; org-todo-line-regexp

;; (defun org-get-todo-state ()
;;   "Return the TODO keyword of the current subtree."
;;   (save-excursion
;;     (org-back-to-heading t)
;;     (and (let ((case-fold-search nil)) (looking-at org-todo-line-regexp))
;;          (match-end 2)
;;          (match-string 2))))


(defun occ-tsk-builder-ORIGINAL ()
  (unless (occ-collections-map-get key)
    (occ-obj-collection-object))
  (if (occ-collections-map-get key)
      (let ((classname (occ-cl-inst-classname (occ-obj-collection-object))))
        (cond
         ((eq 'occ-list-collection classname)
          #'make-occ-list-tsk)
         ((eq 'occ-tree-collection classname)
          #'make-occ-tree-tsk)
         (t
          (occ-error "(occ-collections-map-get key) is not from occ-list-collection or occ-tree-collection class"))))
    (occ-error "(occ-collections-map-get key) is NIL not from occ-list-collection or occ-tree-collection class")))


(defun occ-tsk-builder (key)
  ;; TODO: GLOBAL-TO-LOCAL
  (unless (occ-collections-map-get key)
    (occ-obj-collection-object))
  (if (occ-collections-map-get key)
      (let ((classname (occ-cl-inst-classname (occ-obj-collection-object))))
        (cond
         ((eq 'occ-list-collection classname)
          #'make-occ-list-tsk)
         ((eq 'occ-tree-collection classname)
          #'make-occ-tree-tsk)
         (t
          (occ-error "(occ-collections-map-get key) is not from occ-list-collection or occ-tree-collection class"))))
    (occ-error "(occ-collections-map-get key) is NIL not from occ-list-collection or occ-tree-collection class")))


(defun occ-heading-content-only ()
  (when (org-at-heading-p)
    (save-excursion
      (save-restriction
        (let ((start (progn
                       (goto-char (org-element-property :contents-begin
                                                        (org-element-at-point)))
                       (while (org-at-drawer-p)
                         (goto-char (org-element-property :end
                                                          (org-element-at-point))))
                       (point))))
          (unless (org-at-heading-p)
            (progn
              (outline-next-heading)
              ;; (outline-next-visible-heading 1)
              (backward-char)
              (buffer-substring start (point)))))))))


;; utils
(defun occ-util-keyword2sym (key)
  (key2sym key))

(defun occ-util-plist-mapcar (fun plist)
  "return alist"
  (mapcar fun
          (seq-partition plist 2)))

(defun occ-util-plist-value-mapcar (fun plist)
  (apply #'append
         (occ-plist-mapcar #'(lambda (c)
                               (list (first c)
                                     (funcall fun (nth 1 c))))
                           plist)))

(defun occ-tsk-plist-from-org (plist)
  (let ((ret-plist (apply #'append
                          (occ-util-plist-mapcar #'(lambda (c)
                                                     (list (first c)
                                                           (occ-obj-prop-from-org (occ-util-keyword2sym (first c))
                                                                                  (nth 1 c))))
                                                 plist))))
    (cl-assert (evenp (length     plist)))
    (occ-debug "occ-tsk-plist-from-org: plist %s" plist)
    (cl-assert (evenp (length ret-plist)))
    ret-plist))

(occ-testing
 (eq (aref (symbol-name :test) 0) ?:)
 (list-utils-flatten '((a  b) (x)))
 (seq-partition (list :a 1 :b 2 :c 3 :more (list 4 5 6) :x nil) 2)
 (apply #'append (seq-partition (list :a 1 :b 2 :c 3 :more (list 4 5 6) :x nil) 2)))


;; utils
(defun occ-get-tsk-category (heading plist)
  (if (stringp heading)
      (or (when (string-match "<\\([a-zA-Z][a-zA-Z0-9]+\\)>" heading)
            (match-string 1 heading))
          (plist-get plist :CATEGORY)
          "TODO")
    "TODO"))


(defun occ-obj-make-tsk-at-point-ORIGINAL (&optional builder)
    (let ((builder (or builder
                       (occ-tsk-builder))))
        (let ((tsk nil)
              (heading-with-string-prop
               (if (org-before-first-heading-p)
                   'noheading
                 (org-get-heading 'notags))))
          (let ((heading      (when heading-with-string-prop
                                (if (eq heading-with-string-prop 'noheading)
                                    heading-with-string-prop
                                  (substring-no-properties heading-with-string-prop))))
                (heading-prop heading-with-string-prop)
                (marker       (move-marker (make-marker)
                                           (point)
                                           (org-base-buffer (current-buffer))))
                (file         (buffer-file-name))
                (point        (point))
                (clock-sum    (if (org-before-first-heading-p)
                                  0
                                (org-clock-sum-current-item)))
                ;; BUG: TODO: SHOULD need to maintain plist of :PROPERTIES:
                ;; separately as keys for these are returned in UPCASE. while it
                ;; is not the case with other generic properties which are not
                ;; part of :PROPERTIES: block.

                ;; NOTE also these two are mixed in one list only
                (tsk-plist    (nth 1 (org-element-at-point))))
            (cl-assert (evenp (length tsk-plist)))
            (when heading
              (setf tsk
                    (funcall builder
                             ;; (occ-obj-prop-from-org) from Org world to Occ world.
                             :name         (occ-obj-prop-from-org 'name heading)
                             :heading      (occ-obj-prop-from-org 'heading heading)
                             :heading-prop (occ-obj-prop-from-org 'heading-prop heading-prop)
                             :marker       (occ-obj-prop-from-org 'marker marker)
                             :file         (occ-obj-prop-from-org 'file file)
                             :point        (occ-obj-prop-from-org 'point point)
                             :clock-sum    (occ-obj-prop-from-org 'clock-sum clock-sum)
                             :cat          (occ-obj-prop-from-org 'cat (occ-get-tsk-category heading tsk-plist))
                             :plist        (occ-tsk-plist-from-org tsk-plist)))
              (let ((inherit         t)
                    (inherited-props
                     ;; is it correct ? - guess it is ok and correct.
                     (occ-readprop-props)))
                (dolist (prop inherited-props)
                  (let* ((propstr (if (keywordp prop)
                                      (substring (symbol-name prop) 1)
                                    (symbol-name prop)))
                         (val (org-entry-get nil propstr inherit)))
                    (unless (occ-obj-get-property tsk prop)
                      ;; What is the solution
                      (occ-obj-set-property tsk prop val :not-recursive t)))))
              (progn "set :plist here"))
            (occ-obj-reread-props tsk)      ;reset list properties
            tsk))))

(defun occ-obj-make-tsk-at-point (builder)
  (let ((tsk                      nil)
        (heading-with-string-prop (if (org-before-first-heading-p)
                                      'noheading
                                    (org-get-heading 'notags))))
       (let ((heading      (when heading-with-string-prop
                             (if (eq heading-with-string-prop 'noheading)
                                 heading-with-string-prop
                               (substring-no-properties heading-with-string-prop))))
             (heading-prop heading-with-string-prop)
             (marker       (move-marker (make-marker)
                                        (point)
                                        (org-base-buffer (current-buffer))))
             (file         (buffer-file-name))
             (point        (point))
             (clock-sum    (if (org-before-first-heading-p)
                               0
                             (org-clock-sum-current-item)))
             ;; BUG: TODO: SHOULD need to maintain plist of :PROPERTIES:
             ;; separately as keys for these are returned in UPCASE. while it
             ;; is not the case with other generic properties which are not
             ;; part of :PROPERTIES: block.

             ;; NOTE also these two are mixed in one list only
             (tsk-plist    (nth 1 (org-element-at-point))))
         (cl-assert (evenp (length tsk-plist)))
         (when heading
           (setf tsk
                 (funcall builder
                          ;; (occ-obj-prop-from-org) from Org world to Occ world.
                          :name         (occ-obj-prop-from-org 'name heading)
                          :heading      (occ-obj-prop-from-org 'heading heading)
                          :heading-prop (occ-obj-prop-from-org 'heading-prop heading-prop)
                          :marker       (occ-obj-prop-from-org 'marker marker)
                          :file         (occ-obj-prop-from-org 'file file)
                          :point        (occ-obj-prop-from-org 'point point)
                          :clock-sum    (occ-obj-prop-from-org 'clock-sum clock-sum)
                          :cat          (occ-obj-prop-from-org 'cat (occ-get-tsk-category heading tsk-plist))
                          :plist        (occ-tsk-plist-from-org tsk-plist)))
           (let ((inherit         t)
                 (inherited-props
                  ;; is it correct ? - guess it is ok and correct.
                  (occ-readprop-props)))
             (dolist (prop inherited-props)
               (let* ((propstr (if (keywordp prop)
                                   (substring (symbol-name prop) 1)
                                 (symbol-name prop)))
                      (val (org-entry-get nil propstr inherit)))
                 (unless (occ-obj-get-property tsk prop)
                   ;; What is the solution
                   (occ-obj-set-property tsk prop val :not-recursive t)))))
           (progn "set :plist here"))
         (occ-obj-reread-props tsk)      ;reset list properties
         tsk)))

(defun occ-obj-builder-tsk-at-point (builder)
  #'(lambda ()
      (occ-obj-make-tsk-at-point builder)))

(cl-defmethod occ-obj-make-tsk ((obj number)
                                &optional
                                (key string))
  (occ-debug "point %s" obj)
  (if (<= obj (point-max))
      (save-restriction
        (save-excursion
          (goto-char obj)
          (occ-obj-make-tsk-at-point (occ-tsk-builder (or key "default")))))))

(cl-defmethod occ-obj-make-tsk ((obj marker)
                                &optional
                                (key string))
  (occ-debug "point %s" obj)
  (if (and (marker-buffer obj)
           (numberp       (marker-position obj)))
      (with-current-buffer (marker-buffer obj)
        (if (<= (marker-position obj)
                (point-max))
            (occ-obj-make-tsk (marker-position obj)
                              key)))))

(cl-defmethod occ-obj-make-tsk ((obj null)
                                &optional
                                (key string))
  (occ-debug "current pos %s" (point-marker))
  (occ-obj-make-tsk (point-marker) key))

(cl-defmethod occ-obj-make-tsk ((obj occ-tsk)
                                &optional
                                (key string))
  obj)


(defvar occ-ctx-hash (make-hash-table :test #'equal :size 100 :rehash-size 20))
(defun occ-ctx-puthash (plist ctx)
  (puthash plist ctx occ-ctx-hash))
(defun occ-ctx-gethash (plist)
  (gethash plist occ-ctx-hash))
(defun occ-ctx-remhash (plist)
  (remhash plist occ-ctx-hash))
(defun occ-ctx-clrhash ()
  (clrhash occ-ctx-hash))
(defun occ-ctx-hashlen ()
 (hash-table-count occ-ctx-hash))


(cl-defmethod occ-obj-make-filter (&key average stddev variance)
  (make-occ-filter
   :average  average
   :stddev   stddev
   :variance variance))


(cl-defgeneric occ-obj-make-ctx (obj)
  "occ-obj-make-ctx")

(cl-defmethod occ-obj-make-ctx-at-point (&optional mrk)
  (let* ((mrk   (or mrk (point-marker)))
         (buff  (marker-buffer mrk))
         (buff  (if buff
                    (if (bufferp buff)
                        buff
                      (if (stringp buff)
                          (or (get-buffer buff)
                              (if (file-exists-p buff)
                                  (get-file-buffer buff)))))
                  (window-buffer)))
         (buff  (org-base-buffer buff))
         (file  (buffer-file-name buff))
         (plist (list :name (buffer-name buff)
                      :file file
                      :buffer buff)))
    (unless (occ-ctx-gethash plist)
      (occ-ctx-puthash plist (make-occ-ctx :name (buffer-name buff)
                                           :file file
                                           :buffer buff)))
    (occ-ctx-gethash plist)))

(cl-defmethod occ-obj-make-ctx ((obj buffer))
  (let ((mrk (make-marker)))
    (set-marker mrk 0 obj)
    (occ-obj-make-ctx-at-point mrk)))

(cl-defmethod occ-obj-make-ctx ((obj marker))
  (occ-obj-make-ctx-at-point obj))

(cl-defmethod occ-obj-make-ctx ((obj null))
  (occ-obj-make-ctx-at-point (point-marker)))

(cl-defmethod occ-obj-make-ctx ((obj occ-ctx))
  obj)


(cl-defgeneric occ-obj-make-ctsk-with (tsk ctx)
  "occ-obj-make-ctsk-with")

(cl-defmethod occ-obj-make-ctsk-with ((tsk occ-tsk)
                                      (ctx occ-ctx))
  ;; use occ-obj-build-ctsk-with
  (make-occ-ctsk :name    nil
                 :tsk     tsk
                 :ctx     ctx))

(cl-defmethod occ-obj-make-ctsk ((obj occ-ctsk))
  obj)

(cl-defmethod occ-obj-make-ctsk ((obj occ-ctxual-tsk))
  ;; use occ-obj-build-ctsk-with
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (make-occ-ctsk :name    nil
                   :tsk     tsk
                   :ctx     ctx)))

(cl-defmethod occ-obj-build-ctsk-with ((tsk occ-tsk) ;ctor
                                       (ctx occ-ctx))
  (occ-obj-make-ctsk-with tsk ctx))

(cl-defmethod occ-obj-build-ctsk ((obj occ-ctxual-tsk))
  (occ-obj-make-ctsk obj))

(cl-defmethod occ-obj-build-ctsk ((obj occ-ctsk))
  obj)


(cl-defgeneric occ-obj-make-ctxual-tsk-with (tsk
                                             ctx
                                             &optional
                                             rank)
  "occ-obj-make-ctxual-tsk-with")

(cl-defmethod occ-obj-make-ctxual-tsk-with ((tsk occ-tsk)
                                            (ctx occ-ctx)
                                            &optional
                                            rank)
  ;; use occ-obj-build-ctxual-tsk-with
  (make-occ-ctxual-tsk :name    nil
                       :tsk     tsk
                       :ctx     ctx
                       :rank    rank))

(cl-defmethod occ-obj-build-ctxual-tsk-with ((tsk occ-tsk) ;ctor
                                             (ctx occ-ctx))
  (occ-obj-make-ctxual-tsk-with tsk ctx))

(cl-defmethod occ-obj-build-ctxual-tsk-with ((tsk occ-ctxual-tsk) ;ctor
                                             (ctx occ-ctx))
  (debug))

(cl-defmethod occ-obj-build-ctxual-tsk-with ((tsk null) ;ctor
                                             (ctx occ-ctx))
  nil)

(cl-defmethod occ-obj-make-ctxual-tsk ((obj occ-ctsk)
                                       &optional
                                       rank)
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (make-occ-ctxual-tsk :name    nil
                         :tsk     tsk
                         :ctx     ctx
                         :rank    rank)))

(cl-defmethod occ-obj-make-ctxual-tsk ((obj occ-ctxual-tsk)
                                       &optional
                                       rank)
  obj)

(cl-defmethod occ-obj-build-ctxual-tsk ((obj occ-ctsk)
                                        &optional
                                        rank)
  (occ-obj-make-ctxual-tsk obj rank))

(cl-defmethod occ-obj-build-ctxual-tsk ((obj occ-ctxual-tsk)
                                        &optional
                                        rank)
  obj)


(cl-defmethod occ-obj-build-obj-with ((obj occ-tsk)
                                      (ctx occ-ctx))
   (occ-obj-build-ctxual-tsk-with obj ctx))

(cl-defmethod occ-obj-build-obj-with ((obj occ-tsk)
                                      (ctx null))
  (occ-obj-build-obj-with obj (occ-obj-make-ctx-at-point)))


(cl-defmethod occ-obj-make-tsk-collection ((key string)
                                           (file-spec (head :tree)))
  (unless (occ-collections-map-get key)
    (let ((collection (make-occ-tree-collection :name  "tsk collection tree"
                                                :roots (rest file-spec))))
      (occ-collections-map-set key collection))))

(cl-defmethod occ-obj-make-tsk-collection ((key string)
                                           (file-spec (head :list)))
  (unless (occ-collections-map-get key)
    (let ((collection (make-occ-list-collection :name  "tsk collection list"
                                                :roots (rest dir-spec))))
      (setf (occ-collections-map-set key collection)))))


(defun occ-obj-make-return (label
                            value)
  (make-occ-return :label label
                   :value value))

;; ctors
(defun occ-obj-make-callable-normal (keyword
                                     name
                                     fun)
  "Dynamic object"
  (make-occ-callable-normal :keyword keyword
                            :name    name
                            :fun     fun))

(defun occ-obj-make-callable-generator (keyword
                                        name
                                        fun)
  "Dynamic object"
  (make-occ-callable-generator :keyword keyword
                               :name    name
                               :fun     fun))

(defun occ-obj-build-callable-normal (keyword
                                      name
                                      fun)
  "Callable creation and to be stored via (OCC-HELM-CALLABLE-ADD CALLABLE)"
  (let ((callable (occ-obj-make-callable-normal keyword
                                            name
                                            fun)))
    (occ-helm-callable-add callable)
    callable))

(defun occ-obj-build-callable-generator (keyword
                                         name
                                         fun)
  "Callable creation and to be stored via (OCC-HELM-CALLABLE-ADD CALLABLE)"
  (let ((callable (occ-obj-make-callable-generator keyword
                                               name
                                               fun)))
    (occ-helm-callable-add callable)
    callable))


;; ctors
(cl-defmethod occ-obj-make-ap-normal ((ap-obj list))
  (make-occ-ap-normal :tree-keybranch
                      ap-obj))

(cl-defmethod occ-obj-make-ap-normal ((ap-obj occ-ap-normal))
  ap-obj)

(cl-defmethod occ-obj-make-ap-normal ((ap-obj (head :tree-keybranch)))
  (let ((tree-keybranch (rest ap-obj)))
    (make-occ-ap-normal :tree-keybranch
                        tree-keybranch)))

(cl-defmethod occ-obj-make-ap-normal ((ap-obj (head :callables)))
  (let ((callables (rest ap-obj)))
    (make-occ-ap-normal :callables
                        (occ-obj-callables callables nil))))

(cl-defmethod occ-obj-make-ap-normal ((ap-obj (head :keywords)))
  (let* ((keywords  (rest ap-obj))
         (callables (occ-helm-callables-get keywords)))
   (make-occ-ap-normal :callables
                       callables)))


(cl-defmethod occ-obj-make-ap-transf ((ap-obj list))
  (make-occ-ap-transf :tree-keybranch ap-obj))

(cl-defmethod occ-obj-make-ap-transf ((ap-obj occ-ap-normal))
  (let ((callables (occ-ap-normal-callables ap-obj)))
    (cl-assert callables t "ap-obj should have callable")
    (make-occ-ap-transf :callables
                        (occ-obj-callables callables nil))))

(cl-defmethod occ-obj-make-ap-transf ((ap-obj occ-ap-transf))
  ap-obj)

(cl-defmethod occ-obj-make-ap-transf ((ap-obj (head :tree-keybranch)))
  (let ((tree-keybranch (rest ap-obj)))
    (make-occ-ap-transf :tree-keybranch
                        tree-keybranch)))

(cl-defmethod occ-obj-make-ap-transf ((ap-obj (head :callables)))
  (let ((callables (rest ap-obj)))
    (make-occ-ap-transf :callables
                        callables)))

(cl-defmethod occ-obj-make-ap-transf ((ap-obj (head :keywords)))
  (let* ((keywors   (rest ap-obj))
         (callables (occ-helm-callables-get keywords)))
    (make-occ-ap-transf :callables
                        callables)))

(cl-defmethod occ-obj-make-ap-transf ((ap-obj (head :transform)))
  (let ((transform (rest ap-obj)))
    (make-occ-ap-transf :transform
                       transform)))


;; ctors
(cl-defmethod occ-obj-build-ap-normal ((ap-obj list)
                                       &optional
                                       optional-obj)
  (make-occ-ap-normal :tree-keybranch ap-obj))

(cl-defmethod occ-obj-build-ap-normal ((ap-obj (head :tree-keybranch))
                                       &optional
                                       optional-obj)
  (occ-obj-make-ap-normal ap-obj))

(cl-defmethod occ-obj-build-ap-normal ((ap-obj (head :callables))
                                       &optional
                                       optional-obj)
  (occ-obj-make-ap-normal ap-obj))

(cl-defmethod occ-obj-build-ap-normal ((ap-obj (head :keywords))
                                       &optional
                                       optional-obj)
  (occ-obj-make-ap-normal ap-obj))

(cl-defmethod occ-obj-build-ap-normal ((ap-obj occ-ap-normal)
                                       &optional
                                       optional-obj)
  ap-obj)

(cl-defmethod occ-obj-build-ap-normal ((ap-obj null)
                                       &optional
                                       optional-obj)
  (occ-obj-make-ap-normal optional-obj))


(cl-defmethod occ-obj-build-ap-transf ((ap-obj list)
                                       &optional
                                       optional-obj)
  (occ-obj-make-ap-transf ap-obj))

(cl-defmethod occ-obj-build-ap-transf ((ap-obj (head :tree-keybranch))
                                       &optional
                                       optional-obj)
  (occ-obj-make-ap-transf ap-obj))

(cl-defmethod occ-obj-build-ap-transf ((ap-obj (head :callables))
                                       &optional
                                       optional-obj)
  (occ-obj-make-ap-transf ap-obj))

(cl-defmethod occ-obj-build-ap-transf ((ap-obj (head :keywords))
                                       &optional
                                       optional-obj)
  (occ-obj-make-ap-transf ap-obj))

(cl-defmethod occ-obj-build-ap-transf ((ap-obj (head :transform))
                                       &optional
                                       optional-obj)
  (occ-obj-make-ap-transf ap-obj))

(cl-defmethod occ-obj-build-ap-transf ((ap-obj occ-ap-normal)
                                       &optional
                                       optional-obj)
  (occ-obj-make-ap-transf ap-obj))

(cl-defmethod occ-obj-build-ap-transf ((ap-obj occ-ap-transf)
                                       &optional
                                       optional-obj)
  ap-obj)

(cl-defmethod occ-obj-build-ap-transf ((ap-obj null)
                                       &optional
                                       optional-obj)
  (if optional-obj
      (occ-obj-build-ap-transf optional-obj)))

;; ctor

(defvar occ-return-select-label     :occ-selected    "should not be null")
(defvar occ-return-quit-label       :occ-nocandidate "should not be null")
(defvar occ-return-nocndidate-label :occ-quitted     "should not be null")
(defvar occ-return-timeout-label    :occ-timeout     "should not be null") ;TODO: need to implement.
(defvar occ-return-true-label       :occ-true        "should not be null")
(defvar occ-return-false-label      :occ-false       "should not be null")
(defvar occ-return-evaluate         :occ-eval        "should not be null")

(cl-assert occ-return-select-label)
(cl-assert occ-return-quit-label)
(cl-assert occ-return-nocndidate-label)
(cl-assert occ-return-true-label)
(cl-assert occ-return-false-label)

(defvar occ-return-select-function #'identity)
(defvar occ-return-select-name     "Select")
(cl-assert occ-return-select-function)
(cl-assert occ-return-select-name)

(fmakunbound 'occ-obj-build-return-lambda)
(cl-defmethod occ-obj-build-return-lambda ((callable occ-callable-normal)
                                           &optional
                                           label)
  (let ((newcallable #'(lambda (candidate)
                         (let ((fun (occ-callable-fun callable)))
                           (let* ((value (funcall fun candidate))
                                  (label (if (or (null label)
                                                 (eq label occ-return-evaluate))
                                             (if value
                                                 occ-return-true-label
                                               occ-return-false-label)
                                           label)))
                             (occ-obj-make-return label value)))))
        (name        (occ-callable-name callable))
        (keyword     (occ-callable-keyword callable)))
    (occ-obj-make-callable-normal keyword
                              name
                              newcallable)))

(cl-defmethod occ-obj-build-return-lambda ((callable occ-callable-generator)
                                           &optional
                                           label)
  (occ-error "Can not use occ-callable-transf %s" callable))


(occ-testing
 (let* ((obj       (occ-obj-make-ctx-at-point))
        (ap-obj    (occ-obj-make-ap-transf '(t actions general)))
        (transform (occ-obj-ap-helm-item ap-obj obj)))
   transform)

 ())

;;; occ-obj-ctor.el ends here
