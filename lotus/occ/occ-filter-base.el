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
;; (defun occ-obj-filters-get (keylist)
;;   (mapcan #'(lambda (key)
;;               (cl-remove-if-not #'(lambda (filter)
;;                                     (eq key
;;                                         (occ-filter-keyword filter)))
;;                                 occ-obj-filters))
;;           keylist))


;; (cl-defmethod occ-obj-get-filters ((obj occ-obj-ctx)
;;                                    keylist)
;;   ;; TODO: do we require (apply #'append ...)
;;   (ignore obj)
;;   (occ-debug "(OCC-OBJ-GET-FILTERS OCC-OBJ-TSK): called")
;;   (occ-obj-filters-get keylist))


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


(cl-defmethod occ-obj-apply-recursively ((obj occ-ctx)
                                         methods
                                         sequence
                                         &key rank)
  "Main engine it applies filters METHODS recursively to SEQUENCE"
  (let* ((funkw-rank (cl-first methods))) ;check occ-filter-config.el
    (let ((funkw (or (car-safe funkw-rank)
                     funkw-rank))
          (rank  (if (consp funkw-rank)
                     (nth 1 funkw-rank)
                   rank)))
      (occ-debug "occ-obj-apply-recursively: trying funkw-rank = %s funkw = %s"
                   funkw-rank
                   funkw)
      (occ-debug "occ-obj-apply-recursively: received (length sequence) = %d"
                   (length sequence))
      (if funkw
          (let ((fun (or (occ-filter-fun (occ-obj-static-filter-get funkw))
                         ;; funkw
                         #'identity)))
            (occ-debug "occ-obj-apply-recursively: calling fun = %s rank = %s with (length sequence) = %d"
                         fun
                         rank
                         (length sequence))
            (occ-obj-apply-recursively obj
                                       (cl-rest methods)
                                       (funcall fun obj
                                                sequence
                                                :rank rank)
                                       :rank rank))
        sequence))))

(cl-defmethod occ-obj-filter ((obj occ-ctx)
                              methods
                              sequence
                              &key rank)
  (let* ((rank (or rank
                   #'occ-obj-rank))
         (seq  (occ-obj-apply-recursively obj
                                          methods
                                          sequence
                                          :rank rank)))
    ;; (occ-debug "occ-obj-filter: (length seq) = %d"
    ;;              (length seq))
    ;; (occ-debug "occ-obj-filter: seq = %s"
    ;;              seq)
    seq))


(defmethod occ-obj-points ((filter occ-filter) seq)
  (funcall (occ-filter-point-gen-fn filter) seq))
(defmethod occ-obj-cmp ((filter occ-filter))
  (occ-filter-comparator filter))


(cl-defmethod occ-obj-filter ((obj occ-ctx)
                              methods
                              sequence
                              &key rank)
  (let* ((rank (or rank
                   #'occ-obj-rank))
         (seq  (occ-obj-apply-recursively obj
                                          methods
                                          sequence
                                          :rank rank)))
    ;; (occ-debug "occ-obj-filter: (length seq) = %d"
    ;;              (length seq))
    ;; (occ-debug "occ-obj-filter: seq = %s"
    ;;              seq)
    seq))


(cl-defmethod occ-obj-static-filter-points ((static-filter occ-static-filter)
                                     (obj occ-ctx)
                                     sequence
                                     :rank rank)
  (let ((points-gen-fn (occ-static-filter-points-gen-fn static-filter)))
    (funcall points-gen-fn obj
             sequence
             :rank rank)))

(cl-defmethod occ-obj-static-filter-default-pivot ((static-filter occ-static-filter)
                                            (obj occ-ctx)
                                            points)
  (let ((default-pivot-fn (occ-static-filter-default-pivot-fn static-filter)))
    (funcall default-pivot-fn obj
             points)))


(cl-defmethod occ-obj-dyn-filter-seq ((dyn-filter occ-obj-dyn-filter))
  (funcall (occ-dyn-filter-seq-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-filter ((dyn-filter occ-obj-dyn-filter))
  (funcall (occ-dyn-filter-filter-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-points ((dyn-filter occ-obj-dyn-filter))
  (funcall (occ-dyn-filter-points-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-increment ((dyn-filter occ-obj-dyn-filter))
  (funcall (occ-dyn-filter-increment-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-decrement ((dyn-filter occ-obj-dyn-filter))
  (funcall (occ-dyn-filter-decrement-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-reset ((dyn-filter occ-obj-dyn-filter))
  (funcall (occ-dyn-filter-reset-closure-fn dyn-filter)))


(cl-defmethod occ-obj-get-dyn-filter ((filter occ-filter)
                                      (obj occ-ctx)
                                      prev
                                      sequence
                                      &key rank)
  (let* ((rank          (or rank
                            #'occ-obj-rank))
         (points        (occ-obj-filter-points filter
                                               obj
                                               sequence
                                               :rank rank))
         (default-pivot (occ-obj-filter-default-pivot filter
                                                      obj
                                                      points))
         (pivot         default-pivot))
    (let* ((seq-closure-fn       (if prev-dyn-filter
                                     (occ-obj-dyn-filter-filter-closure prev-dyn-filter)
                                     #'(lambda () sequence)))
           (filter-closure-fn    #'(lambda ()
                                     (cl-remove-if-not (lambda (s)
                                                         (funcall (occ-filter-compare-fn filter)
                                                                  (funcall rank s)
                                                                  (nth pivot points)))
                                                       (funcall seq-closure-fn))))
           (increment-closure-fn #'(lambda ()
                                     (setf pivot (mod (1+ pivot)
                                                      (length points)))))
           (decrement-closure-fn #'(lambda ()
                                     (setf pivot (mod (1- pivot)
                                                      (length points)))))
           (reset-closure-fn     #'(lambda ()
                                     (setf pivot default-pivot))))
      (occ-obj-build-dyn-filter (occ-obj-name filter)
                                :seq-closure-fn seq-closure-fn
                                :filter-closure-fn filter-closure-fn
                                :increment-closure-fn increment-closure-fn
                                :decrement-closure-fn decrement-closure-fn
                                :reset-closure-fn reset-closure-fn
                                :prev prev))))

(cl-defmethod occ-obj-filter-recursive ((obj occ-ctx)
                                        methods
                                        sequence
                                        &key rank)
  (let* ((filterkw-rank (cl-first methods))
         (filter        (occ-obj-static-filter-get (or (car-safe filterkw-rank)
                                                       filterkw-rank)))
         ;; (filter        (occ-obj-filter-get :incremental))
         (rank          (if (consp filterkw-rank)
                            (nth 1 filterkw-rank)
                          (or rank
                              #'occ-obj-rank))))
    (let* ((prev (if (cdr methods)
                     (occ-obj-filter-recursive obj
                                               (cdr methods)
                                               sequence
                                               :rank rank)
                   nil))
           (next (occ-obj-get-dyn-filter filter
                                         obj
                                         sequence
                                         prev
                                         :rank rank)))
      (when prev
        (setf (occ-dyn-filter-next prev) next))
      next)))

(defun occ-obj-build-helm-command (closure-fn)
  #'(lambda ()
      (interactive)
      (funcall closure-fn)
      (helm-refresh)))

(cl-defmethod occ-obj-filter-ops ((obj occ-ctx)
                                  methods
                                  sequence
                                  &key rank)
  (let ((current-dyn-filter (occ-obj-filter-recursive obj
                                                      methods
                                                      sequence :rank rank)))
    (let ((seq-closure-fn       #'(lambda () (occ-obj-dyn-filter-seq current-dyn-filter)))
          (filter-closure-fn    #'(lambda () (occ-obj-dyn-filter-filter  current-dyn-filter)))
          (increment-closure-fn (occ-obj-build-helm-command #'(lambda () (occ-obj-dyn-filter-increment current-dyn-filter))))
          (decrement-closure-fn (occ-obj-build-helm-command #'(lambda () (occ-obj-dyn-filter-decrement current-dyn-filter))))
          (reset-closure-fn     (occ-obj-build-helm-command #'(lambda () (occ-obj-dyn-filter-reset current-dyn-filter))))
          (curr-closure-fn      #'(lambda () current-dyn-filter))
          (prev-closure-fn      (occ-obj-build-helm-command #'(lambda ()
                                                                (let ((prev (occ-dyn-filter-prev current-dyn-filter)))
                                                                  (if prev
                                                                      (setf current-dyn-filter prev)
                                                                    (beep)
                                                                    (occ-message "No prev"))))))
          (next-closure-fn      (occ-obj-build-helm-command #'(lambda ()
                                                                (let ((next (occ-dyn-filter-next current-dyn-filter)))
                                                                  (if next
                                                                      (setf current-dyn-filter next)
                                                                    (beep)
                                                                    (occ-message "No next"))))))))
    (make-occ-combined-dyn-filter :name "Test"
                                  :seq-closure-fn       seq-closure-fn
                                  :filter-closure-fn    filter-closure-fn
                                  :increment-closure-fn increment-closure-fn
                                  :decrement-closure-fn decrement-closure-fn
                                  :reset-closure-fn     reset-closure-fn
                                  :curr-closure-fn      curr-closure-fn
                                  :prev-closure-fn      prev-closure-fn
                                  :next-closure-fn      next-closure-fn)))

;; (occ-obj-get-filters (occ-obj-make-ctx-at-point) (occ-match-filters))
;; (occ-obj-filters-get (occ-match-filters))
;; (occ-obj-filters-get '(:identity :positive))

;;; occ-filter-base.el ends here
