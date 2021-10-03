;;; occ-list-filter.el --- list filter               -*- lexical-binding: t; -*-

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

(provide 'occ-list-filter)


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
  (first (remove-if-not #'(lambda (filter)
                            (eq key
                                (occ-filter-keyword filter)))
                      occ-obj-filters)))
(defun occ-obj-filters-get (keylist)
  (mapcan #'(lambda (key)
              (remove-if-not #'(lambda (filter)
                                 (eq key
                                     (occ-filter-keyword filter)))
                             occ-obj-filters))
          keylist))


(cl-defmethod occ-obj-get-filters ((obj occ-obj-ctx)
                                   keylist)
  ;; TODO: do we require (apply #'append ...)
  (occ-message "(OCC-OBJ-GET-FILTERS OCC-OBJ-TSK): called")
  (occ-obj-filters-get keylist))



(cl-defmethod occ-obj-average ((obj occ-stat) sequence)
  (unless (occ-stat-average obj)
    (setf (occ-stat-average obj) (apply #'occ-stats-average sequence)))
  (occ-stat-average obj))

(cl-defmethod occ-obj-stddev ((obj occ-stat) sequence)
  (unless (occ-stat-stddev obj)
    (setf (occ-stat-stddev obj) (apply #'occ-stats-stddev sequence)))
  (occ-stat-stddev obj))

(cl-defmethod occ-obj-variance ((obj occ-stat) sequence)
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


;; (occ-generate-plist-functions occ-obj filter)

;; (occ-helm-actions-get )

;; (defvar occ-obj-filters-plist nil)

;; (defun occ-filter-add (key fun)
;;   (setq occ-obj-filters-plist
;;         (plist-put
;;          occ-filters-plist
;;          key fun)))

;; (defun occ-filter-get (key)
;;   (plist-get occ-obj-filters-plist key))

;; occ-obj-filter-add
;; occ-obj-filter-set
;; occ-obj-filter-get

(defun occ-filters-get (&rest keys)
  (let ((funs nil))
    (dolist (key keys)
      (let ((funkw-rank keys))
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


;; (defun occ-internal-get-filter-method (methods)
;;   (cond
;;    ((functionp methods)
;;     (occ-internal-get-filter-method (funcall methods)))
;;    ((and (symbolp   methods)
;;          (listp (symbol-value methods)))
;;     (occ-internal-get-filter-method (symbol-value methods)))
;;    ((and (symbolp   methods)
;;          (functionp (symbol-value methods)))
;;     (occ-internal-get-filter-method (functionp (symbol-value methods))))
;;    ((listp methods) methods)
;;    (t (occ-error "Wrong %s methods" methods))))


(cl-defmethod occ-obj-apply-recursively ((obj occ-ctx)
                                         methods
                                         sequence
                                         &key rank)
  (let* ((funkw-rank (first methods)))
    (let ((funkw      (or (car-safe funkw-rank)
                          funkw-rank))
          (rank       (if (consp funkw-rank)
                          (nth 1 funkw-rank)
                        rank)))
     (occ-message "occ-obj-apply-recursively: trying funkw-rank= %s funkw= %s" funkw-rank funkw)
     (if funkw
         (let ((fun (or (occ-filter-fun (occ-obj-filter-get funkw))
                        ;; funkw
                        #'identity)))
           (occ-obj-apply-recursively obj
                                      (rest methods)
                                      (funcall fun obj
                                               sequence
                                               :rank rank)
                                      :rank rank))
       sequence))))

(cl-defmethod occ-obj-filter ((obj occ-ctx)
                              methods
                              sequence
                              &key rank)
  (let ((rank (or rank
                  #'occ-obj-rank)))
    (occ-obj-apply-recursively obj
                               methods
                               sequence
                               :rank rank)))


(cl-defmethod occ-obj-filter-mutual-deviation ((obj occ-ctx)
                                               sequence
                                               &key rank) ;TODO: make it after method
  "Return matched Sequence for context CTX"
  (if (occ-obj-collection-object)
      (let* ((rankslist  (mapcar #'occ-obj-rank       sequence))
             (avgrank    (apply  #'occ-stats-average  rankslist))
             (varirank   (apply  #'occ-stats-variance rankslist)))
        ;; (occ-debug "occ-collection-obj-matches :around finish")
        (occ-debug "matched ctxtsks %s" (length sequence))
        (occ-debug "occ-filter-mutual-deviation: avgrank = %d varirank = %d"
                          avgrank varirank)
        (remove-if-not #'(lambda (obj)
                           (>= (funcall rank obj)
                               avgrank))
                       sequence))
    (occ-error "(occ-obj-collection-object) returned nil")))



(cl-defmethod occ-obj-filter-positive ((obj occ-ctx)
                                       sequence
                                       &key rank)
  (remove-if-not #'(lambda (obj)
                     (> (funcall rank obj)
                        0))
                 sequence))




(cl-defmethod occ-obj-filter-nonnegative ((obj occ-ctx)
                                          sequence
                                          &key rank)
  (remove-if-not #'(lambda (obj)
                     (>= (funcall rank obj)
                         0))
                 sequence))



(defvar occ-filter-min 0)
(cl-defmethod occ-obj-filter-min ((obj occ-ctx)
                                  sequence
                                  &key rank)
  (remove-if-not #'(lambda (obj)
                     (>= (funcall rank obj)
                         occ-filter-min))
                 sequence))



(defvar occ-filter-max 0)
(cl-defmethod occ-obj-filter-max ((obj occ-ctx)
                                  sequence
                                  &key rank)
  (remove-if-not #'(lambda (obj)
                     (>= (funcall rank obj)
                         occ-filter-max))
                 sequence))




(defun occ-filter-config-initialize ()
  (occ-obj-build-filter :mutual-deviation "Mutual Deviation" #'occ-obj-filter-mutual-deviation)
  (occ-obj-build-filter :positive "Positive" #'occ-obj-filter-positive)
  (occ-obj-build-filter :nonnegative "Non negative" #'occ-obj-filter-nonnegative)
  (occ-obj-build-filter :min "Minimum" #'occ-obj-filter-min)
  (occ-obj-build-filter :max "Maximum" #'occ-obj-filter-max))


(defun occ-list-filters ()
  '(:nonnegative))

(defun occ-match-filters ()
  (list :positive
        :mutual-deviation
        (list :positive #'occ-obj-member-tsk-rank)))
        ;; (list :mutual-deviation #'occ-obj-member-tsk-rank)

(defun occ-never-filters ()
  "Used to filter mainly non-tsk"
  '(:nonnegative))

;;; occ-list-filter.el ends here
