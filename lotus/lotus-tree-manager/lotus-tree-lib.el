;;; lotus-tree-lib.el --- Lotus Tree Library         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <spratap@merunetworks.com>
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

(provide 'lotus-tree-lib)


;; old
(defun collect-carlist (alist)
  (let ((ulist nil))
    (dolist (pair (copy-tree alist))
      (if (assoc (first pair) ulist)
          (nconc (assoc (first pair) ulist) (rest pair))
        (setf ulist (append ulist (list pair)))))
    ulist))

;; new
(defun collect-carlist (alist)
  (let ((ulist nil))
    (dolist (pair alist)
      (unless (assoc (first pair) ulist)
        (when (first pair)
          (push (list (first pair)) ulist)))
      (let ((ef (first pair)))
        (let ((xulist (assoc (first pair) ulist))
              (tlist  nil)
              (ilist  (rest pair)))
          (dolist (i ilist)
            (unless (member i (rest xulist))
              (push i tlist)))
          (setq tlist (nreverse tlist))
          (nconc (assoc (first pair) ulist) tlist))))
    (setq ulist (nreverse ulist))))


(defun collect-alist (alist)
  (let ((ulist nil))
    (dolist (pair alist)
      (unless (assoc (first pair) ulist)
        (when (first pair)
          (push (list (first pair)) ulist)))
      (nconc (assoc (first pair) ulist) (list (rest pair))))
    (setq ulist (nreverse ulist))))


(defun delete-dups-alist (alist)
  (dolist (pair alist)
    (setcdr pair (delete-dups (rest pair))))
  alist)


(defun max-depth (tree &optional nodep)
  (let ((nodep (or nodep #'atom)))
    (if (funcall nodep tree)
        0
      (1+ (reduce #'max
                  (mapcar #'(lambda (subtree)
                              (if (consp subtree)
                                  (max-depth subtree nodep)
                                0))
                          tree))))))

(defun collect-elem-cond (tree nodep predicate)
  (if (funcall nodep tree)
      tree
    (when (funcall predicate tree)
      (remove nil
              (mapcar #'(lambda (subtree)
                          (collect-elem-cond subtree nodep predicate))
                      (rest tree))))))

(defun collect-elem-cond-depth (tree nodep predicate depth)
  (collect-elem-cond tree
                     nodep
                     #'(lambda (subtree)
                         (or
                          (<= (max-depth subtree nodep) depth)
                          (funcall predicate subtree)))))

(defun collect-elem-simple-depth (tree depth)
  (collect-elem-cond-depth tree
                           #'(lambda (x)
                               (not (listp x)))
                           #'(lambda (subtree)
                               (memq (first subtree) '(t)))
                           depth))


(defun tree-add (keys item list)
  (let ((key (first keys)))
    (if (rest list)
        (if key
            (progn
              (unless (assoc key (rest list))
                (nconc (rest list) (list (list key))))
              (tree-add (rest keys) item (assoc key (rest list))))
          (unless (memq item (rest list))
            (nconc (rest list) (list item))))
      (progn
        (when key
          (nconc list (list (list key))))
        (if (rest keys)
            (tree-add (rest keys) item (assoc key (rest list)))
          (if key
              (nconc (assoc key (rest list)) (list item))
            (nconc list (list item))))))))

;;;###autoload
(defun tree-add (keys item list)
  (let ((key (first keys)))
    (if (rest list)
        (if key
            (progn
              (unless (assoc key (rest list))
                (nconc (rest list) (list (list key))))
              (tree-add (rest keys) item (assoc key (rest list))))
          (unless (member item (rest list))
            (nconc (rest list) (list item))))
      (progn
        (when key
          (nconc list (list (list key))))
        (if (rest keys)
            (tree-add (rest keys) item (assoc key (rest list)))
          (if key
              (nconc (assoc key (rest list)) (list item))
            (nconc list (list item))))))))


;;;###autoload
(defun tree-flatten (predicate L)
  ;; https://stackoverflow.com/a/19967639
  "Converts a list to single level."
  (let ((predicate (or predicate #'atom)))
   (if (null L)
       nil
     (if (funcall predicate (first L))
         (cons (first L)
               (tree-flatten predicate (rest L)))
       (append (tree-flatten predicate (first L))
               (tree-flatten predicate (rest L)))))))


(when nil
 (setq org-capture+-helm-templates-tree '(t))
 (tree-add '(x z) 'y org-capture+-helm-templates-tree)
 (tree-add '(x z) 'k org-capture+-helm-templates-tree)
 (tree-add '(a z) 'k org-capture+-helm-templates-tree)
 (tree-add '(x n) 'i org-capture+-helm-templates-tree)
 (tree-add '(x n b) 'i org-capture+-helm-templates-tree)
 (tree-add '(x #'(lambda () t) x) 'c org-capture+-helm-templates-tree))

;;; lotus-tree-lib.el ends here
