;;; occ-filter-base.el --- list filter               -*- lexical-binding: t; -*-

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

(provide 'occ-filter-base)


(require 'occ-obj-common)
(require 'occ-tree)
(eval-when-compile
  (require 'occ-macros))
(require 'occ-macros)
(require 'occ-obj-accessor)
(require 'occ-obj-utils)
(require 'occ-util-common)
(require 'occ-print)
(require 'occ-predicate)
(require 'occ-rank)
(require 'occ-statistics)



(defvar occ-obj-static-filters nil)
(defun occ-obj-static-filter-add (static-filter)
  (cl-pushnew static-filter
              occ-obj-static-filters))
(defun occ-obj-static-filter-get (key)
  (cl-first (cl-remove-if-not #'(lambda (filter)
                                  (eq key
                                      (occ-static-filter-keyword filter)))
                              occ-obj-static-filters)))
(defun occ-obj-static-filters-get (keylist)
  (mapcan #'(lambda (key)
              (let ((fkey (or (car-safe key)
                              key)))
                (cl-remove-if-not #'(lambda (filter)
                                      (eq fkey
                                          (occ-static-filter-keyword filter)))
                                  occ-obj-static-filters)))
          keylist))


(cl-defmethod occ-obj-get-static-filters ((obj occ-obj-ctx)
                                          keylist)
  ;; TODO: do we require (apply #'append ...)
  (ignore obj)
  (occ-debug "(OCC-OBJ-GET-FILTERS OCC-OBJ-TSK): called")
  (occ-obj-static-filters-get keylist))

;; (occ-obj-get-static-filters (occ-obj-make-ctx-at-point) (occ-match-filters))
;; (occ-obj-static-filters-get (occ-match-filters))
;; (occ-obj-static-filters-get '(:identity :positive))


(cl-defmethod occ-obj-average ((obj occ-stat)
                               sequence)
  (unless (occ-stat-average obj)
    (setf (occ-stat-average obj) (apply #'occ-stats-average sequence)))
  (occ-stat-average obj))

(cl-defmethod occ-obj-stddev ((obj occ-stat)
                              sequence)
  (unless (occ-stat-stddev obj)
    (setf (occ-stat-stddev obj) (apply #'occ-stats-stddev sequence)))
  (occ-stat-stddev obj))

(cl-defmethod occ-obj-variance ((obj occ-stat)
                                sequence)
  (unless (occ-stat-variance obj)
    (setf (occ-stat-variance obj) (apply #'occ-stats-variance sequence)))
  (occ-stat-variance obj))


(cl-defmethod occ-obj-ctx-stat ((obj occ-obj-ctx)
                                stat)
  (unless (plist-get (occ-obj-ctx-stat-plist obj)
                     stat)
    (plist-put (occ-obj-ctx-stat-plist obj) stat
               (occ-obj-make-stat)))
  (plist-get (occ-obj-ctx-stat-plist obj) stat))


;; (defmethod occ-obj-points ((filter occ-static-filter) seq)
;;   (funcall (occ-static-filter-points-gen-fn filter) seq))
;; (defmethod occ-obj-cmp ((filter occ-static-filter))
;;   (occ-static-filter-compare-fn filter))


(cl-defmethod occ-obj-static-filter-points ((static-filter occ-static-filter)
                                            (obj occ-ctx)
                                            sequence
                                            &key rank)
  (let ((points-gen-fn (occ-static-filter-points-gen-fn static-filter)))
    (funcall points-gen-fn
             obj
             sequence
             :rank rank)))

(cl-defmethod occ-obj-static-filter-default-pivot ((static-filter occ-static-filter)
                                                   (obj occ-ctx)
                                                   points)
  (let ((default-pivot-fn (occ-static-filter-default-pivot-fn static-filter)))
    (funcall default-pivot-fn obj
             points)))


(cl-defmethod occ-obj-dyn-filter-seq ((dyn-filter occ-obj-dyn-filter))
  (funcall (occ-obj-dyn-filter-seq-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-filter ((dyn-filter occ-obj-dyn-filter))
  (funcall (occ-obj-dyn-filter-filter-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-points ((dyn-filter occ-obj-dyn-filter))
  (funcall (occ-obj-dyn-filter-points-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-increment ((dyn-filter occ-obj-dyn-filter))
  (funcall (occ-obj-dyn-filter-increment-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-decrement ((dyn-filter occ-obj-dyn-filter))
  (funcall (occ-obj-dyn-filter-decrement-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-reset ((dyn-filter occ-obj-dyn-filter))
  (funcall (occ-obj-dyn-filter-reset-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-next-closure-fn ((dyn-filter occ-combined-dyn-filter))
  (occ-combined-dyn-filter-next-closure-fn dyn-filter))

(cl-defmethod occ-obj-dyn-filter-prev-closure-fn ((dyn-filter occ-combined-dyn-filter))
  (occ-combined-dyn-filter-prev-closure-fn dyn-filter))

;; (cl-defmethod occ-obj-dyn-filter-next-closure-fn ((dyn-filter occ-combined-dyn-filter))
;;   (occ-combined-dyn-filter-next-closure-fn dyn-filter))


(cl-defmethod occ-obj-static-to-dyn-filter ((static-filter occ-static-filter)
                                            (obj occ-ctx)
                                            (sequence list)
                                            prev ;; (prev occ-dyn-filter)
                                            &key rank)
  (occ-debug "occ-obj-static-to-dyn-filter in %s 1" (occ-obj-name static-filter))
  (let* ((rank          (or rank
                            #'occ-obj-rank))
         (points        (occ-obj-static-filter-points static-filter
                                                      obj
                                                      sequence
                                                      :rank rank))
         (default-pivot (occ-obj-static-filter-default-pivot static-filter
                                                             obj
                                                             points))
         (pivot         default-pivot))
    (occ-obj-build-dyn-filter (occ-obj-name static-filter)
                              :seq-closure-fn       (if prev
                                                        (occ-obj-dyn-filter-filter-closure-fn prev)
                                                      #'(lambda () sequence))
                              :filter-closure-fn    #'(lambda ()
                                                        (cl-remove-if-not #'(lambda (ctsk)
                                                                              (funcall (occ-static-filter-compare-fn static-filter)
                                                                                       (funcall rank ctsk)
                                                                                       (nth pivot points)))
                                                                          (if prev
                                                                              (occ-obj-dyn-filter-filter prev)
                                                                            sequence)))
                              :increment-closure-fn #'(lambda ()
                                                        (setf pivot (mod (1+ pivot)
                                                                         (length points))))
                              :decrement-closure-fn #'(lambda ()
                                                        (setf pivot (mod (1- pivot)
                                                                         (length points))))
                              :reset-closure-fn     #'(lambda ()
                                                        (setf pivot default-pivot))
                              :prev                 prev)))

(cl-defmethod occ-obj-build-dyn-filters-recursive ((obj occ-ctx)
                                                   (static-filter-methods list)
                                                   (sequence list)
                                                   &key rank)
  (let* ((static-filterkw-rank (cl-first static-filter-methods))
         (static-filter (occ-obj-static-filter-get (or (car-safe static-filterkw-rank)
                                                       static-filterkw-rank)))
         (rank          (if (consp static-filterkw-rank)
                            (nth 1 static-filterkw-rank)
                          (or rank
                              #'occ-obj-rank))))
    (occ-assert static-filter)
    (occ-debug "occ-obj-build-dyn-filters-recursive in")
    (let* ((prev (if (cdr static-filter-methods)
                     (occ-obj-build-dyn-filters-recursive obj
                                                          (cdr static-filter-methods)
                                                          sequence
                                                          :rank rank)
                   nil)))
      (occ-obj-static-to-dyn-filter static-filter
                                    obj
                                    sequence
                                    prev
                                    :rank rank))))

(defun occ-obj-build-helm-command (closure-fn)
  #'(lambda ()
      (interactive)
      (funcall closure-fn)
      (helm-refresh)))

(cl-defmethod occ-obj-combined-dyn-filter ((obj occ-ctx)
                                           (static-filter-methods list)
                                           (sequence list)
                                           &key rank)
  (occ-debug "occ-obj-combined-dyn-filter: Going in")
  (let ((curr-dyn-filter (occ-obj-build-dyn-filters-recursive obj
                                                              static-filter-methods ;; (list :incremental);; static-filter-methods
                                                              sequence :rank rank))
        (stack nil))
    (occ-debug "occ-obj-combined-dyn-filter: Coming out")
    (occ-debug "occ-obj-combined-dyn-filter: curr-dyn-filter %s" (occ-obj-name curr-dyn-filter))
    (occ-obj-build-combined-dyn-filter "CTX"
                                       :curr-closure-fn      #'(lambda () curr-dyn-filter)
                                       :prev-closure-fn      (occ-obj-build-helm-command #'(lambda ()
                                                                                             (let ((prev (occ-dyn-filter-prev curr-dyn-filter)))
                                                                                               (if prev
                                                                                                   (progn
                                                                                                     (occ-message "Setting prev %s" (occ-obj-name prev))
                                                                                                     (push curr-dyn-filter stack)
                                                                                                     (setf curr-dyn-filter prev))
                                                                                                 (ding t)
                                                                                                 (occ-message "No prev")))))
                                       :next-closure-fn      (occ-obj-build-helm-command #'(lambda ()
                                                                                             (if stack
                                                                                                 (let ((next (pop stack)))
                                                                                                   (occ-message "Setting next %s" (occ-obj-name next))
                                                                                                   (setf curr-dyn-filter next))
                                                                                               (ding t)
                                                                                               (occ-message "No next"))))
                                       :seq-closure-fn       #'(lambda ()
                                                                 (occ-obj-dyn-filter-seq curr-dyn-filter))
                                       :filter-closure-fn    #'(lambda () (occ-obj-dyn-filter-filter  curr-dyn-filter))
                                       :increment-closure-fn (occ-obj-build-helm-command #'(lambda () (occ-obj-dyn-filter-increment curr-dyn-filter)))
                                       :decrement-closure-fn (occ-obj-build-helm-command #'(lambda () (occ-obj-dyn-filter-decrement curr-dyn-filter)))
                                       :reset-closure-fn     (occ-obj-build-helm-command #'(lambda () (occ-obj-dyn-filter-reset curr-dyn-filter))))))

;;; occ-filter-base.el ends here
