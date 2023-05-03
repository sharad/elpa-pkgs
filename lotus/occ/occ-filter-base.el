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



(defvar occ-obj-filters nil)
(defun occ-obj-filter-add (filter)
  (push filter
        occ-obj-filters))
(defun occ-obj-filter-get (key)
  (cl-first (cl-remove-if-not #'(lambda (filter)
                                  (eq key
                                      (occ-filter-keyword filter)))
                              occ-obj-filters)))
(defun occ-obj-filters-get (keylist)
  (mapcan #'(lambda (key)
              (cl-remove-if-not #'(lambda (filter)
                                    (eq key
                                        (occ-filter-keyword filter)))
                                occ-obj-filters))
          keylist))


(cl-defmethod occ-obj-get-filters ((obj occ-obj-ctx)
                                   keylist)
  ;; TODO: do we require (apply #'append ...)
  (ignore obj)
  (occ-debug "(OCC-OBJ-GET-FILTERS OCC-OBJ-TSK): called")
  (occ-obj-filters-get keylist))



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


;; (defun occ-filters-get (&rest keys)
;;   (let ((funs nil))
;;     (dolist (key keys)
;;       (ignore key)
;;       (let ((funkw-rank keys))
;;         (let ((funkw (or (car-safe funkw-rank)
;;                          funkw-rank))
;;               (rank  (if (consp funkw-rank)
;;                          (nth 1 funkw-rank)
;;                        nil)))
;;           (when funkw
;;             (let ((fun (or (occ-filter-fun (occ-obj-filter-get funkw))
;;                            funkw
;;                            #'identity)))
;;               (setf funs (nconc funs
;;                                 (list (if rank ;; (consp funkw-rank)
;;                                           (list fun rank)
;;                                         fun)))))))))
;;     funs))

(defun occ-filters-get (&rest keys)
  (let ((funs nil))
    (dolist (key keys)
      (let ((funkw-rank key))
        (let ((funkw (or (car-safe funkw-rank)
                         funkw-rank))
              (rank  (if (consp funkw-rank)
                         (nth 1 funkw-rank)
                       nil)))
          (when funkw
            (let ((fun (or (occ-filter-fun (occ-obj-filter-get funkw))
                           funkw
                           #'identity)))
              (setf funs (nconc funs
                                (list (if rank ;; (consp funkw-rank)
                                          (list fun rank)
                                        fun)))))))))
    funs))


(cl-defmethod occ-obj-apply-recursively ((obj occ-ctx)
                                         methods
                                         sequence
                                         &key rank)
  "Main engine it applies filters METHODS recursively to SEQUENCE"
  (let* ((funkw-rank (cl-first methods)))
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
          (let ((fun (or (occ-filter-fun (occ-obj-filter-get funkw))
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

;;; occ-filter-base.el ends here
