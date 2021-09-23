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


(cl-defmethod occ-obj-average ((obj occ-filter) sequence)
  (unless (occ-obj-filter-average obj)
    (setf (occ-obj-filter-average obj) (apply #'occ-stats-average sequence)))
  (occ-obj-filter-average obj))

(cl-defmethod occ-obj-stddev ((obj occ-filter) sequence)
  (unless (occ-obj-filter-stddev obj)
    (setf (occ-obj-filter-stddev obj) (apply #'occ-stats-stddev sequence)))
  (occ-obj-filter-stddev obj))

(cl-defmethod occ-obj-variance ((obj occ-filter) sequence)
  (unless (occ-obj-filter-variance obj)
    (setf (occ-obj-filter-variance obj) (apply #'occ-stats-variance sequence)))
  (occ-obj-filter-variance obj))


(cl-defmethod occ-obj-ctx-filter ((obj occ-obj-ctx) filter)
  (unless (plist-get (occ-obj-ctx-filter-plist obj) filter)
    (plist-put (occ-obj-ctx-filter-plist obj) filter (occ-obj-make-filter)))
  (plist-get (occ-obj-ctx-filter-plist obj) filter))


(occ-generate-plist-functions occ-obj filter)

;; (occ-helm-actions-get )

;; (defvar occ-obj-filters-plist nil)

;; (defun occ-filter-add (key fun)
;;   (setq occ-filters-plist
;;         (plist-put
;;          occ-filters-plist
;;          key fun)))

;; (defun occ-filter-get (key)
;;   (plist-get occ-filters-plist key))

(defun occ-filters-get (&rest keys)
  (let ((funs nil))
    (dolist (key keys)
      (let ((funkw-rank keys))
        (let ((funkw (or (car-safe funkw-rank) funkw-rank))
              (rank  (if (consp funkw-rank) (nth 1 funkw-rank) nil)))
          (when funkw
              (let ((fun (or (occ-obj-filter-get funkw) funkw #'identity)))
                (setf funs (nconc funs
                                  (list (if rank ;; (consp funkw-rank)
                                            (list fun rank)
                                          fun)))))))))
    funs))


(defun occ-internal-get-filter-method (methods)
  (cond
   ((functionp methods)
    (occ-internal-get-filter-method (funcall methods)))
   ((and (symbolp   methods)
         (listp (symbol-value methods)))
    (occ-internal-get-filter-method (symbol-value methods)))
   ((and (symbolp   methods)
         (functionp (symbol-value methods)))
    (occ-internal-get-filter-method (functionp (symbol-value methods))))
   ((listp methods) methods)
   (t (occ-error "Wrong %s methods" methods))))


(cl-defmethod occ-obj-apply-recursively ((obj occ-ctx)
                                         methods
                                         sequence
                                         &key rank)
  (let* ((funkw-rank (first methods)))
    (let ((funkw      (or (car-safe funkw-rank) funkw-rank))
          (rank       (if (consp funkw-rank) (nth 1 funkw-rank) rank)))
     ;; (occ-message "occ-obj-apply-recursively: trying funkw-rank= %s funkw= %s" funkw-rank funkw)
     (if funkw
         (let ((fun  (or (rest (occ-obj-filter-get funkw))
                         funkw
                         #'identity)))
           (occ-obj-apply-recursively obj
                                  (rest methods)
                                  (funcall fun obj sequence :rank rank)
                                  :rank rank))
       sequence))))

(cl-defmethod occ-obj-filter ((obj occ-ctx)
                              methods
                              sequence
                              &key rank)
  (let ((rank    (or rank #'occ-obj-rank)))
    (occ-obj-apply-recursively obj
                               methods
                               sequence
                               :rank rank)))


(cl-defmethod occ-obj-filter-mutual-deviation ((obj occ-ctx)
                                               sequence
                                               &key rank) ;TODO: make it after method
  "Return matched Sequence for context CTX"
  (if (occ-obj-collection-object)
      (let* ((rankslist  (mapcar #'occ-obj-rank sequence))
             (avgrank    (apply #'occ-stats-average rankslist))
             (varirank   (apply #'occ-stats-variance rankslist)))
        ;; (occ-debug "occ-collection-obj-matches :around finish")
        (occ-debug "matched ctxtsks %s" (length sequence))
        (occ-debug "occ-filter-mutual-deviation: avgrank = %d varirank = %d"
                          avgrank varirank)
        (remove-if-not
         #'(lambda (obj)
             (>= (funcall rank obj) avgrank))
         sequence))
    (occ-error "(occ-obj-collection-object) returned nil")))

(occ-obj-filter-add :mutual-deviation "Mutual Deviation" #'occ-obj-filter-mutual-deviation)

(cl-defmethod occ-obj-filter-positive ((obj occ-ctx)
                                       sequence
                                       &key rank)
  (remove-if-not #'(lambda (obj) (> (funcall rank obj) 0))
                 sequence))

(occ-obj-filter-add :positive "Positive" #'occ-obj-filter-positive)


(cl-defmethod occ-obj-filter-nonnegative ((obj occ-ctx)
                                          sequence
                                          &key rank)
  (remove-if-not #'(lambda (obj) (>= (funcall rank obj) 0))
                 sequence))

(occ-obj-filter-add :nonnegative "Non negative" #'occ-obj-filter-nonnegative)

(defvar occ-filter-min 0)
(cl-defmethod occ-obj-filter-min ((obj occ-ctx)
                                  sequence
                                  &key rank)
  (remove-if-not #'(lambda (obj) (>= (funcall rank obj) occ-filter-min))
                 sequence))

(occ-obj-filter-add :min "Minimum" #'occ-obj-filter-min)

(defvar occ-filter-max 0)
(cl-defmethod occ-obj-filter-max ((obj occ-ctx)
                                  sequence
                                  &key rank)
  (remove-if-not #'(lambda (obj)
                     (>= (funcall rank obj)
                         occ-filter-max))
                 sequence))

(occ-obj-filter-add :max "Maximum" #'occ-obj-filter-max)


(defun occ-list-filters ()
  '(:nonnegative))

(defun occ-match-filters ()
  (list :positive
        :mutual-deviation
        (list :positive         #'occ-obj-member-tsk-rank)))
        ;; (list :mutual-deviation #'occ-obj-member-tsk-rank)

(defun occ-never-filters ()
  "Used to filter mainly non-tsk"
  '(:nonnegative))

;;; occ-list-filter.el ends here
