;;; occ-tree-tsk.el --- tree tsk                     -*- lexical-binding: t; -*-

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

(provide 'occ-tree-tsk)


(require 'occ-util-common)
(require 'occ-tree)
(require 'occ-obj-ctor)


(defun occ-tree-tsk-node-p (tx)
  "Test org TX is org tsks tree non-leaf node"
  (occ-obj-get-property tx 'subtree))

(defun occ-tree-tsk-subtree (tx)
  "Test org TX is org tsks tree non-leaf node"
  (occ-obj-get-property tx 'subtree))

(defun occ-mapcar-tree-tsks (fn tree args)
  "Tree mapcar return result for FN for all TREE nodes with ARGS"
  (tree-mapcar-nodes
   'occ-tree-tsk-subtree fn tree args))

(defun occ-mapc-tree-tsks (fn tree args)
  "Tree mapc run FN for all TREE nodes with ARGS"
  (tree-mapc-nodes
   'occ-tree-tsk-subtree fn tree args))

(defun occ-remove-if-not-tree-tsks (fn tree args)
  "Tree remove if return TREE with all node and its subtree removed if node return nil for PREDICATE"
  (tree-remove-if-not-nodes
   'occ-tree-tsk-subtree fn tree args))


(defun occ-org-map-subheading (fun)
  "Call FUN for every heading underneath the current heading"
  ;; (org-back-to-heading)
  (let ((level (funcall outline-level))
        (collection nil))
    (save-excursion
      (while (and (progn
                    (outline-next-heading)
                    (> (funcall outline-level)
                       level))
                  (not (eobp)))
        (if (= (funcall outline-level)
               (1+ level))
            (setf collection
                  (nconc collection (list (funcall fun)))))))
    collection))


;; TODO: ADD support for non existing file. and suppose it buffer is present but not file.
;; should work with any changes
(defun occ-tree-tsk-build (&optional
                           file
                           collection
                           subtree-level)
  "Build recursive org tsks from org FILE (or current buffer) using TSK-BUILDER-AT-POINT function e.g. occ-collect-tsk"
  (save-excursion
    (save-restriction
      (with-current-buffer (if file
                               (occ-find-file-noselect file)
                             (current-buffer))
        (let ((subtree-level          (or subtree-level
                                          1))
              (tsk-builder-at-point   (occ-obj-tsk-builder-at-point collection)))
          (when (and (buffer-live-p (current-buffer))
                     (> (buffer-size (current-buffer))
                        30))
            (occ-setup-buffer)
            (when file
              (unless (string= file (buffer-file-name (current-buffer)))
                (error "file `%s' and current file %s not same."
                       file
                       (buffer-file-name (current-buffer)))))
            (unless (eq major-mode 'org-mode)
              (error "Not in `%s' org buffer" (current-buffer)))
            (when file
              (goto-char (point-min)))
            ;; here many time if other call thread come then current buffer gets changed cause issue with tsk-builder-at-point
            (let ((entry         (funcall tsk-builder-at-point))
                  (subtree-level (if subtree-level subtree-level 1)))
              (when (numberp subtree-level)
                (occ-obj-set-property entry 'subtree-level
                                      subtree-level))
              (cl-assert (numberp subtree-level))
              (when entry
                (let* ((sub-tree (append (occ-org-map-subheading #'(lambda ()
                                                                     (occ-tree-tsk-build nil
                                                                                         collection
                                                                                         subtree-level)))
                                         (let ((subtree-file-prop (occ-obj-get-property entry :SUBTREEFILE)))
                                           (when subtree-file-prop
                                             (let* ((file         (if file file (buffer-file-name)))
                                                    (subtree-file (if (and subtree-file-prop
                                                                           (file-relative-name subtree-file-prop))
                                                                      (expand-file-name subtree-file-prop
                                                                                        (if file
                                                                                            (file-name-directory file)
                                                                                          default-directory))
                                                                    subtree-file)))
                                               (if (and subtree-file
                                                        (file-readable-p subtree-file))
                                                   (list (occ-tree-tsk-build subtree-file
                                                                             collection
                                                                             (+ 1
                                                                                (or (occ-obj-get-property entry
                                                                                                          'level)
                                                                                    0)
                                                                                (or subtree-level
                                                                                    0)))))))))))
                  (when sub-tree (occ-obj-set-property entry 'subtree
                                                       sub-tree))
                  entry)))))))))

(cl-defmethod occ-obj-drived-tsk-builder ((collection occ-tree-collection))
  #'(lambda (&optional file)
      (occ-tree-tsk-build file
                          collection
                          1)))

(cl-defmethod occ-obj-build-tsks ((collection occ-tree-collection))
  (let ((builder (occ-obj-drived-tsk-builder collection)))
    (remove nil (mapcar builder
                        (occ-tree-collection-roots collection)))))

;;; occ-tree-tsk.el ends here
