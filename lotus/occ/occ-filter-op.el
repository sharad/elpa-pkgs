;;; occ-filter-op.el --- occ filter operations       -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sharad

;; Author: sharad <spratap@merunetworks.com>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:


(provide 'occ-filter-op)


(require 'occ-filter-base)


(cl-defmethod occ-obj-filter-mutual-deviation ((obj occ-ctx)
                                               sequence
                                               &key rank) ;TODO: make it after method
  "Return matched Sequence for context CTX"
  (if (occ-default-collection)
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
    (occ-error "(occ-default-collection) returned nil")))

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

;;; occ-filter-op.el ends here
