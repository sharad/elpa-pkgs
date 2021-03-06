;;; occ-obj.el --- occ-api               -*- lexical-binding: t; -*-
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

;;

;;; Code:

(provide 'occ-obj)



;; * cl-macs
;; https://nullprogram.com/blog/2018/02/14/
;;
;; * TODO org-base-buffer
;;
;; https://stackoverflow.com/questions/12262220/add-created-date-property-to-todos-in-org-mode
;;
;; https://stackoverflow.com/questions/40884764/lisp-get-all-slot-names-from-an-class-instance


(defvar occ-verbose 0)

;;
;;                                                                                               obj
;;                                                                                                |
;;                                       +--------------------------------------------------------+--------------------------------------------------------+---------------------------------------+
;;                                       |                                                        |                                                        |                                       |
;;                                    obj-tsk                                                 obj-prop                                                  obj-ctx                                  filter
;;                                       |                                                        |                                                        |
;;          +----------------------------+----------------------------+                         prop                               +-----------------------+-----------------------+
;;          |                                                         |                                                            |                                               |
;;     obj-ctx-tsk                                                   tsk                                                    obj-collection                                        ctx
;;          |                                                         |                                                            |
;;        ctsk                                             +----------+----------+                                      +----------+----------+
;;          |                                              |                     |                                      |                     |
;;      ctxual-tsk                                      tree-tsk              list-tsk                           list-collection         tree-collection
;;


(cl-defstruct occ-obj
  name)

(cl-defstruct (occ-obj-tsk (:include occ-obj))
  "Will hold tsk ctx obj-ctx-tsk")

(cl-defstruct (occ-obj-ctx-tsk (:include occ-obj-tsk))
  "Will hold ctsk occ-ctxual-tsk")

;; (cl-defstruct (occ-obj-prop (:include occ-obj))
;;   "Will hold prop")

(cl-defstruct (occ-filter (:include occ-obj))
  average
  stddev
  variance)

(cl-defstruct (occ-obj-ctx (:include occ-obj))
  "Will hold ctx"
  filter-plist)

(cl-defstruct (occ-obj-collection (:include occ-obj-ctx))
  "Will hold collection")


;; (cl-defstruct (occ-prop (:include occ-obj-prop))
;;   value)

;; NOTE: Remember when adding new attributes, nned to destroy existing object, else it will cause miss-match.
(cl-defstruct (occ-tsk (:include occ-obj-tsk))
  ;; [[file:~/.repos/git/main/resource/info/doc/orgs/private/doc/contents/org/tasks/personal/works/emacs/todo.org::*Each%20task%20should%20have%20different%20types%20of%20actions%20associated%20to%20it,%20default%20is%20to%20clock-in%20to%20it][Each task should have different types of actions associated to it, default is to clock-in to it]]
  action
  ;; *** Each task should have different types of actions associated to it, default is to clock-in to it
  ;; - rest could be
  ;; to ignore this task continue where already clockin or not
  ;; which could be used to avoid non-interesting buffers
  ;; - etc many different could be thought
  heading
  heading-prop
  marker
  file
  point
  clock-sum
  file-level
  cat
  plist
  format-file
  format-string
  ;; rank
  rank)

(cl-defstruct (occ-tree-tsk (:include occ-tsk))
  subtree)

(cl-defstruct (occ-list-tsk (:include occ-tsk))
  )


(cl-defstruct (occ-ctx (:include occ-obj-ctx))
  buffer
  file)


(cl-defstruct (occ-ctsk (:include occ-obj-ctx-tsk))
  ;; Reason to have one more occ-ctsk along with occ-ctxual-tsk to avoid calculating rank.
  ctx
  tsk)

(cl-defstruct (occ-ctxual-tsk (:include occ-ctsk))
  rank)


(cl-defstruct (occ-collection (:include occ-obj-collection))
  roots
  files
  ;; TODO: implement it
  file-lcp)

(cl-defstruct (occ-list-collection (:include occ-collection))
  list)

(cl-defstruct (occ-tree-collection (:include occ-list-collection))
  tree)


(cl-defstruct occ-return
  label
  value)


;;; occ-obj.el ends here
